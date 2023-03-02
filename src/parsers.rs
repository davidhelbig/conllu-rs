use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{self, digit1, newline, tab},
    character::complete::{alpha1, satisfy},
    combinator::{map_res, opt, recognize},
    error::{convert_error, FromExternalError, ParseError, VerboseError},
    multi::{many0, many1_count, many_till, separated_list1},
    sequence::{preceded, separated_pair, tuple},
    Compare, Finish, IResult, InputTake, Parser,
};
use std::{io::BufRead, num::ParseIntError};

use crate::{Features, ParseUposError, Sentence, Token, TokenID, UPOS};

fn parse_digit<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, usize, E> {
    map_res(digit1, |s: &str| s.parse::<usize>())(input)
}

fn parse_single_id<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&str, TokenID, E> {
    let (input, id) = parse_digit(input)?;

    Ok((input, TokenID::Single(id)))
}

fn parse_id_range<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&str, TokenID, E> {
    let (input, (id1, id2)) = separated_pair(parse_digit, tag("-"), parse_digit)(input)?;

    Ok((input, TokenID::Range(id1, id2)))
}

fn parse_id<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&str, TokenID, E> {
    alt((parse_id_range, parse_single_id))(input)
}

fn till_tab<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    take_till(|c| c == '\t')(input)
}

fn till_newline<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    take_till(|c| c == '\n')(input)
}

fn parse_feature<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, (&str, &str), E> {
    let key = recognize(many1_count(satisfy(|c| {
        c.is_alphabetic() || c == '[' || c == ']'
    })));
    let value = recognize(many1_count(satisfy(|c| c.is_alphanumeric() || c == ',')));
    separated_pair(key, tag("="), value)(input)
}

fn parse_features<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&str, Option<Features>, E> {
    match tag("_")(input) {
        Ok((tail, _)) => Ok((tail, None)),
        Err(err) => match separated_list1(tag("|"), parse_feature)(input) {
            Ok((tail, features)) => {
                let features: Features = features
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v.to_string()))
                    .collect();
                Ok((tail, Some(features)))
            }
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    let (tail, comment) = preceded(tag("# "), till_newline)(input)?;
    let (tail, _) = tag("\n")(tail)?;

    Ok((tail, comment))
}

fn text_comment(input: &str) -> IResult<&str, &str> {
    let (tail, comment) = comment(input)?;
    preceded(tag("text = "), till_newline)(input)
}

fn alphalower1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    recognize(many1_count(satisfy(|c| c.is_ascii_lowercase())))(input)
}

fn parse_deprel<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, Option<&str>, E> {
    let mut deprel = recognize(tuple((
        alphalower1,
        opt(tuple((complete::char(':'), alphalower1))),
    )));

    placeholder(deprel).parse(input)
}

fn parse_head<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&str, Option<TokenID>, E> {
    placeholder(parse_id).parse(input)
}

fn placeholder<I: InputTake + Compare<&'static str> + Copy, O, E: ParseError<I>>(
    mut parser: impl Parser<I, O, E>,
) -> impl Parser<I, Option<O>, E> {
    move |input| match tag::<&str, I, E>("_").parse(input) {
        Ok((tail, _)) => Ok((tail, None)),
        Err(err1) => match parser.parse(input) {
            Ok((tail, deprel)) => Ok((tail, Some(deprel))),
            Err(err2) => Err(err2),
        },
    }
}

fn token<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseUposError>,
>(
    input: &'a str,
) -> IResult<&str, Token, E> {
    let (input, (id, _, form, _, lemma, _, upos, _, pos, _, features, _, head, _, deprel, _, _)) =
        tuple((
            parse_id,
            tab,
            till_tab,
            tab,
            lemma,
            tab,
            pos,
            tab,
            till_tab,
            tab,
            parse_features,
            tab,
            parse_head,
            tab,
            parse_deprel,
            till_newline,
            newline,
        ))(input)?;
    Ok((
        input,
        Token {
            id,
            form: form.to_string(),
            lemma: lemma.map(|l| l.to_string()),
            upos,
            features,
            head,
            deprel: deprel.map(|d| d.to_string()),
        },
    ))
}

fn lemma<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, Option<&str>, E> {
    let (tail, token) = till_tab(input)?;

    match token {
        "_" => Ok((tail, None)),
        _ => Ok((tail, Some(token))),
    }
}

fn sentence<
    'a,
    E: ParseError<&'a str>
        + FromExternalError<&'a str, ParseIntError>
        + FromExternalError<&'a str, ParseUposError>,
>(
    input: &'a str,
) -> IResult<&str, Sentence, E> {
    let (input, comments) = many0(comment)(input)?;
    let (input, (tokens, tail)) = many_till(token, newline)(input)?;

    Ok((input, Sentence { tokens }))
}

fn pos<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseUposError>>(
    input: &'a str,
) -> IResult<&str, Option<UPOS>, E> {
    match map_res(alpha1, |s: &str| s.parse::<UPOS>())(input) {
        Ok((tail, upos)) => Ok((tail, Some(upos))),
        Err(nom::Err::Error(err)) => match tag("_")(input) {
            Ok((tail, _)) => Ok((tail, None)),
            Err(err2) => Err(err2),
        },
        Err(err) => Err(err),
    }
}

pub struct Doc<T: BufRead> {
    reader: T,
}

impl<T: BufRead> Doc<T> {
    pub fn new(reader: T) -> Self {
        return Doc { reader };
    }
}

impl<T: BufRead> Iterator for Doc<T> {
    type Item = Result<Sentence, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buffer = String::new();

        // try to read a line from the buffer
        // if we read 0 bytes, we are at EOF and stop the iteration
        // by returning None
        let mut bytes = self.reader.read_line(&mut buffer).unwrap();
        if bytes == 0 {
            return None;
        }

        // fill the buffer until we are at a sentence break
        // or at the end of the file
        // while !buffer.ends_with("\n\n") && bytes != 0 {
        //     bytes = self.reader.read_line(&mut buffer).unwrap();
        // }
        loop {
            bytes = self.reader.read_line(&mut buffer).unwrap();
            if buffer.ends_with("\n\n") {
                break;
            }
            // at EOF, the buffer terminates with a single newline.
            // To treat them equally with other sentences finishing in
            // a double newline, add one here.
            if bytes == 0 {
                buffer.push('\n');
                break;
            }
        }
        match sentence::<VerboseError<&str>>(&buffer).finish() {
            Ok((_, s)) => Some(Ok(s)),
            Err(err) => Some(Err(convert_error(buffer.as_str(), err))),
        }
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use nom::error::ErrorKind;

    use super::*;

    #[test]
    fn parse_pos() {
        assert_eq!(pos::<(&str, ErrorKind)>("_"), Ok(("", None)));
        assert_eq!(pos::<(&str, ErrorKind)>("ADJ"), Ok(("", Some(UPOS::ADJ))));
    }

    #[test]
    fn can_parse_id_range() {
        assert_eq!(
            parse_id_range::<(&str, ErrorKind)>("26-27"),
            Ok(("", TokenID::Range(26, 27)))
        );
    }

    #[test]
    fn can_parse_id() {
        assert_eq!(
            parse_id::<(&str, ErrorKind)>("26-27"),
            Ok(("", TokenID::Range(26, 27)))
        );
    }

    #[test]
    fn can_parse_features() {
        let mut expected = HashMap::new();
        expected.insert("key".to_string(), "value".to_string());
        assert_eq!(
            parse_features::<(&str, ErrorKind)>("key=value"),
            Ok(("", Some(expected)))
        );

        assert_eq!(parse_features::<(&str, ErrorKind)>("_"), Ok(("", None)));
    }

    #[test]
    fn can_parse_alpha_lower() {
        assert_eq!(alphalower1::<(&str, ErrorKind)>("abc"), Ok(("", "abc")));
        assert_eq!(
            alphalower1::<(&str, ErrorKind)>("abcABC"),
            Ok(("ABC", "abc"))
        );
    }

    #[test]
    fn can_parse_deprel() {
        assert_eq!(
            parse_deprel::<(&str, ErrorKind)>("acl"),
            Ok(("", Some("acl")))
        );
        assert_eq!(
            parse_deprel::<(&str, ErrorKind)>("acl:relcl"),
            Ok(("", Some("acl:relcl")))
        );
        assert_eq!(parse_deprel::<(&str, ErrorKind)>("_"), Ok(("", None)));
    }
}
