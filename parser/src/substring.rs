use derivre::{ExprRef, RegexAst, RegexBuilder};
use std::{collections::HashMap, vec};

#[derive(Debug)]
struct State<'a> {
    len: usize,
    link: Option<usize>,
    next: HashMap<&'a str, usize>,
}

/// For details see https://en.wikipedia.org/wiki/Suffix_automaton.
/// Implementation is based on https://cp-algorithms.com/string/suffix-automaton.html
struct SuffixAutomaton<'a> {
    states: Vec<State<'a>>,
    last: usize,
}

impl<'a> SuffixAutomaton<'a> {
    fn new() -> Self {
        let init_state = State {
            len: 0,
            link: None,
            next: HashMap::new(),
        };
        SuffixAutomaton {
            states: vec![init_state],
            last: 0,
        }
    }

    fn from_string(s: &'a str, words: bool) -> Self {
        let mut sa = SuffixAutomaton::new();
        let tokens = if words {
            tokenize_words(s)
        } else {
            tokenize_chars(s)
        };
        for s in tokens.into_iter() {
            sa.extend(s);
        }
        sa
    }

    fn extend(&mut self, s: &'a str) {
        let cur_index = self.states.len();
        self.states.push(State {
            len: self.states[self.last].len + 1,
            link: None,
            next: HashMap::new(),
        });

        let mut p = Some(self.last);
        while let Some(pp) = p {
            if self.states[pp].next.contains_key(&s) {
                break;
            }
            self.states[pp].next.insert(s, cur_index);
            p = self.states[pp].link;
        }

        if let Some(pp) = p {
            let q = self.states[pp].next[&s];
            if self.states[pp].len + 1 == self.states[q].len {
                self.states[cur_index].link = Some(q);
            } else {
                let clone_index = self.states.len();
                self.states.push(State {
                    len: self.states[pp].len + 1,
                    link: self.states[q].link,
                    next: self.states[q].next.clone(),
                });
                while let Some(ppp) = p {
                    if self.states[ppp].next[&s] == q {
                        self.states[ppp].next.insert(s, clone_index);
                    } else {
                        break;
                    }
                    p = self.states[ppp].link;
                }
                self.states[q].link = Some(clone_index);
                self.states[cur_index].link = Some(clone_index);
            }
        } else {
            self.states[cur_index].link = Some(0);
        }
        self.last = cur_index;
    }
}

pub fn substring(
    builder: &mut RegexBuilder,
    string: &str,
    words: bool,
) -> Result<ExprRef, anyhow::Error> {
    let sa = SuffixAutomaton::from_string(string, words);
    let mut state_stack = vec![0];
    let mut node_cache: HashMap<usize, ExprRef> = HashMap::new();

    while let Some(state_index) = state_stack.last() {
        let state = &sa.states[*state_index];
        if node_cache.contains_key(state_index) {
            state_stack.pop();
            continue;
        }

        if state.next.is_empty() {
            let expr = builder.mk(&RegexAst::EmptyString)?;
            node_cache.insert(*state_index, expr);
            state_stack.pop();
            continue;
        }

        let unprocessed_children = state
            .next
            .values()
            .filter(|child_index| !node_cache.contains_key(child_index))
            .collect::<Vec<_>>();
        if !unprocessed_children.is_empty() {
            state_stack.extend(unprocessed_children);
            continue;
        }

        let mut options = state
            .next
            .keys()
            .map(|c| {
                RegexAst::Concat(vec![
                    RegexAst::Literal(c.to_string()),
                    RegexAst::ExprRef(node_cache[&state.next[c]]),
                ])
            })
            .collect::<Vec<_>>();
        options.push(RegexAst::EmptyString);
        let node = RegexAst::Or(options);
        let expr = builder.mk(&node)?;
        node_cache.insert(*state_index, expr);
        state_stack.pop();
    }
    Ok(node_cache[&0])
}

fn tokenize_chars(input: &str) -> Vec<&str> {
    let mut tokens = vec![];
    let mut char_indices = input.char_indices().peekable();

    while let Some((start, _)) = char_indices.next() {
        let end = match char_indices.peek() {
            Some(&(next_index, _)) => next_index,
            None => input.len(),
        };
        tokens.push(&input[start..end]);
    }

    tokens
}

fn tokenize_words(input: &str) -> Vec<&str> {
    let rx = regex::Regex::new(r"(\s+|\w+|[^\s\w]+)").unwrap();
    rx.find_iter(input).map(|m| m.as_str()).collect::<Vec<_>>()
}

#[cfg(test)]
mod test {
    use crate::substring::tokenize_words;

    use super::{substring, tokenize_chars};
    use derivre::RegexBuilder;

    #[test]
    fn test_tokenize_chars() {
        let input = "The quick brown fox jumps over the lazy dog.";
        let tokens = tokenize_chars(input);
        assert_eq!(input, tokens.join(""));
        assert_eq!(
            tokens,
            vec![
                "T", "h", "e", " ", "q", "u", "i", "c", "k", " ", "b", "r", "o", "w", "n", " ",
                "f", "o", "x", " ", "j", "u", "m", "p", "s", " ", "o", "v", "e", "r", " ", "t",
                "h", "e", " ", "l", "a", "z", "y", " ", "d", "o", "g", "."
            ]
        );
    }

    #[test]
    fn test_tokenize_chars_unicode() {
        let input = "빠른 갈색 여우가 게으른 개를 뛰어넘었다.";
        let tokens = tokenize_chars(input);
        assert_eq!(input, tokens.join(""));
        assert_eq!(
            tokens,
            vec![
                "빠", "른", " ", "갈", "색", " ", "여", "우", "가", " ", "게", "으", "른", " ",
                "개", "를", " ", "뛰", "어", "넘", "었", "다", "."
            ]
        );
    }

    #[test]
    fn test_tokenize_words() {
        let input = "The quick brown fox jumps over the lazy dog.";
        let tokens = tokenize_words(input);
        assert_eq!(input, tokens.join(""));
        assert_eq!(
            tokens,
            vec![
                "The", " ", "quick", " ", "brown", " ", "fox", " ", "jumps", " ", "over", " ",
                "the", " ", "lazy", " ", "dog", "."
            ]
        );
    }

    #[test]
    fn test_tokenize_words_unicode() {
        let input = "빠른 갈색 여우가 게으른 개를 뛰어넘었다.";
        let tokens = tokenize_words(input);
        assert_eq!(input, tokens.join(""));
        assert_eq!(
            tokens,
            vec![
                "빠른",
                " ",
                "갈색",
                " ",
                "여우가",
                " ",
                "게으른",
                " ",
                "개를",
                " ",
                "뛰어넘었다",
                "."
            ]
        );
    }

    #[test]
    fn test_substring_chars() {
        let mut builder = RegexBuilder::new();
        let expr = substring(
            &mut builder,
            "The quick brown fox jumps over the lazy dog.",
            false,
        )
        .unwrap();
        let regex = builder.to_regex(expr);
        assert_eq!(
            regex
                .clone()
                .is_match("The quick brown fox jumps over the lazy dog."),
            true
        );
        assert_eq!(regex.clone().is_match("The quick brown fox"), true);
        assert_eq!(regex.clone().is_match("he quick brow"), true);
        assert_eq!(regex.clone().is_match("fox jump"), true);
        assert_eq!(regex.clone().is_match("dog."), true);
        assert_eq!(regex.clone().is_match("brown fx"), false);
    }

    #[test]
    fn test_substring_chars_unicode() {
        let mut builder = RegexBuilder::new();
        let expr = substring(
            &mut builder,
            "빠른 갈색 여우가 게으른 개를 뛰어넘었다.",
            false,
        )
        .unwrap();
        let regex = builder.to_regex(expr);
        assert_eq!(
            regex
                .clone()
                .is_match("빠른 갈색 여우가 게으른 개를 뛰어넘었다."),
            true
        );
        assert_eq!(regex.clone().is_match("빠른 갈색 여우가 게으른"), true);
        assert_eq!(regex.clone().is_match("른 갈색 여우"), true);
        assert_eq!(regex.clone().is_match("여우가 게으"), true);
        assert_eq!(regex.clone().is_match("뛰어넘었다."), true);
        assert_eq!(regex.clone().is_match("갈색 여가"), false);
    }

    #[test]
    fn test_substring_words() {
        let mut builder = RegexBuilder::new();
        let expr = substring(
            &mut builder,
            "The quick brown fox jumps over the lazy dog.",
            true,
        )
        .unwrap();
        let regex = builder.to_regex(expr);
        assert_eq!(
            regex
                .clone()
                .is_match("The quick brown fox jumps over the lazy dog."),
            true
        );
        assert_eq!(regex.clone().is_match("The quick brown fox"), true);
        assert_eq!(regex.clone().is_match("he quick brow"), false);
        assert_eq!(regex.clone().is_match("fox jump"), false);
        assert_eq!(regex.clone().is_match("dog."), true);
        assert_eq!(regex.clone().is_match("brown fx"), false);
    }

    #[test]
    fn test_substring_words_unicode() {
        let mut builder = RegexBuilder::new();
        let expr = substring(
            &mut builder,
            "빠른 갈색 여우가 게으른 개를 뛰어넘었다.",
            true,
        )
        .unwrap();
        let regex = builder.to_regex(expr);
        assert_eq!(
            regex
                .clone()
                .is_match("빠른 갈색 여우가 게으른 개를 뛰어넘었다."),
            true
        );
        assert_eq!(regex.clone().is_match("빠른 갈색 여우가 게으른"), true);
        assert_eq!(regex.clone().is_match("른 갈색 여우"), false);
        assert_eq!(regex.clone().is_match("여우가 게으"), false);
        assert_eq!(regex.clone().is_match("뛰어넘었다."), true);
        assert_eq!(regex.clone().is_match("갈색 여가"), false);
    }
}
