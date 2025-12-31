//
//  Hir.swift
//  swift-lexer
//
//  Created by Christophe Bronner on 2025-12-31.
//

import ContainersPreview
import BasicContainers

/// A high-level intermediate representation (HIR) for a regular expression.
///
/// An HIR value is a combination of a [`HirKind`] and a set of [`Properties`].
/// An `HirKind` indicates what kind of regular expression it is (a literal,
/// a repetition, a look-around assertion, etc.), where as a `Properties`
/// describes various facts about the regular expression. For example, whether
/// it matches UTF-8 or if it matches the empty string.
///
/// The HIR of a regular expression represents an intermediate step between
/// its abstract syntax (a structured description of the concrete syntax) and
/// an actual regex matcher. The purpose of HIR is to make regular expressions
/// easier to analyze. In particular, the AST is much more complex than the
/// HIR. For example, while an AST supports arbitrarily nested character
/// classes, the HIR will flatten all nested classes into a single set. The HIR
/// will also "compile away" every flag present in the concrete syntax. For
/// example, users of HIR expressions never need to worry about case folding;
/// it is handled automatically by the translator (e.g., by translating
/// `(?i:A)` to `[aA]`).
///
/// The specific type of an HIR expression can be accessed via its `kind`
/// or `into_kind` methods. This extra level of indirection exists for two
/// reasons:
///
/// 1. Construction of an HIR expression *must* use the constructor methods on
/// this `Hir` type instead of building the `HirKind` values directly. This
/// permits construction to enforce invariants like "concatenations always
/// consist of two or more sub-expressions."
/// 2. Every HIR expression contains attributes that are defined inductively,
/// and can be computed cheaply during the construction process. For example,
/// one such attribute is whether the expression must match at the beginning of
/// the haystack.
///
/// In particular, if you have an `HirKind` value, then there is intentionally
/// no way to build an `Hir` value from it. You instead need to do case
/// analysis on the `HirKind` value and build the `Hir` value using its smart
/// constructors.
///
/// # UTF-8
///
/// If the HIR was produced by a translator with
/// [`TranslatorBuilder::utf8`](translate::TranslatorBuilder::utf8) enabled,
/// then the HIR is guaranteed to match UTF-8 exclusively for all non-empty
/// matches.
///
/// For empty matches, those can occur at any position. It is the
/// responsibility of the regex engine to determine whether empty matches are
/// permitted between the code units of a single codepoint.
///
/// # Stack space
///
/// This type defines its own destructor that uses constant stack space and
/// heap space proportional to the size of the HIR.
///
/// Also, an `Hir`'s `fmt::Display` implementation prints an HIR as a regular
/// expression pattern string, and uses constant stack space and heap space
/// proportional to the size of the `Hir`. The regex it prints is guaranteed to
/// be _semantically_ equivalent to the original concrete syntax, but it may
/// look very different. (And potentially not practically readable by a human.)
///
/// An `Hir`'s `fmt::Debug` implementation currently does not use constant
/// stack space. The implementation will also suppress some details (such as
/// the `Properties` inlined into every `Hir` value to make it less noisy).
public struct Hir: Hashable, Sendable {
	/// The underlying HIR kind.
	public var kind: HirKind
	/// Analysis info about this HIR, computed during construction.
	public var props: Properties
}

/// Methods for accessing the underlying `HirKind` and `Properties`.
public extension Hir {
	/// Consumes ownership of this HIR expression and returns its underlying
	/// `HirKind`.
	var intoKind: HirKind {
		mutating get {
			let tmp = kind
			self = Hir(kind: .empty, props: props)
			return tmp
		}
	}

	//FIXME: Tuple with noncopyable element type 'HirKind' is not supported
	/*
	/// Splits this HIR into its constituent parts.
	///
	/// This is useful because `let Hir { kind, props } = hir;` does not work
	/// because of `Hir`'s custom `Drop` implementation.
	var parts: (HirKind, Properties) {
		consuming get {
			let parts = (kind, props)
			kind = .empty
			props = .empty
			return parts
		}
	}
	 */
}

/// Smart constructors for HIR values.
///
/// These constructors are called "smart" because they do inductive work or
/// simplifications. For example, calling `Hir::repetition` with a repetition
/// like `a{0}` will actually return a `Hir` with a `HirKind::Empty` kind
/// since it is equivalent to an empty regex. Another example is calling
/// `Hir::concat(vec![expr])`. Instead of getting a `HirKind::Concat`, you'll
/// just get back the original `expr` since it's precisely equivalent.
///
/// Smart constructors enable maintaining invariants about the HIR data type
/// while also simultaneously keeping the representation as simple as possible.
public extension Hir {
	/// Returns an empty HIR expression.
	///
	/// An empty HIR expression always matches, including the empty string.
	static let empty = Hir(kind: .empty, props: .empty)

	//FIXME: Using a property with 'borrowing get' as ~Copyable yields '{capture.sub|hirs}.subscript' is borrowed and cannot be consumed
	borrowing func hasGreedyAll() -> Bool {
		switch kind {
		case let .repetition(repetition):
			//static DOT_HIRS: LazyLock<[Hir; 6]> = LazyLock::new(|| {
			//	[
			//		Hir::dot(Dot::AnyChar),
			//		Hir::dot(Dot::AnyByte),
			//		Hir::dot(Dot::AnyByteExceptLF),
			//		Hir::dot(Dot::AnyCharExceptLF),
			//		Hir::dot(Dot::AnyByteExceptCRLF),
			//		Hir::dot(Dot::AnyCharExceptCRLF),
			//	]
			//});
			// let is_dot = DOT_HIRS.contains(&repetition.sub)
			let isDot = true
			let isUnbounded = repetition.max == nil
			let isGreedy = repetition.isGreedy

			return isDot && isUnbounded && isGreedy
		case .empty:
			return false
			//FIXME: Matching a non-'Copyable' value using a case label that has multiple patterns is not implemented
		case .literal:
			return false
		case .class:
			return false
		case .lookahead:
			return false
		case let .capture(capture):
			return capture.sub.hasGreedyAll()
		case let .concat(hirs):
			return hirs.indices.contains { hirs[$0].hasGreedyAll() }
		case let .alternation(hirs):
			return hirs.indices.contains { hirs[$0].hasGreedyAll() }
		}
	}

	/*
	/// Returns an HIR expression that can never match anything. That is,
	/// the size of the set of strings in the language described by the HIR
	/// returned is `0`.
	///
	/// This is distinct from [`Hir::empty`] in that the empty string matches
	/// the HIR returned by `Hir::empty`. That is, the set of strings in the
	/// language describe described by `Hir::empty` is non-empty.
	///
	/// Note that currently, the HIR returned uses an empty character class to
	/// indicate that nothing can match. An equivalent expression that cannot
	/// match is an empty alternation, but all such "fail" expressions are
	/// normalized (via smart constructors) to empty character classes. This is
	/// because empty character classes can be spelled in the concrete syntax
	/// of a regex (e.g., `\P{any}` or `(?-u:[^\x00-\xFF])` or `[a&&b]`), but
	/// empty alternations cannot.
	#[inline]
	pub fn fail() -> Hir {
		let class = Class::Bytes(ClassBytes::empty());
		let props = Properties::class(&class);
		// We can't just call Hir::class here because it defers to Hir::fail
		// in order to canonicalize the Hir value used to represent "cannot
		// match."
		Hir { kind: HirKind::Class(class), props }
	}

	/// Creates a literal HIR expression.
	///
	/// This accepts anything that can be converted into a `Box<[u8]>`.
	///
	/// Note that there is no mechanism for storing a `char` or a `Box<str>`
	/// in an HIR. Everything is "just bytes." Whether a `Literal` (or
	/// any HIR node) matches valid UTF-8 exclusively can be queried via
	/// [`Properties::is_utf8`].
	///
	/// # Example
	///
	/// This example shows that concatenations of `Literal` HIR values will
	/// automatically get flattened and combined together. So for example, even
	/// if you concat multiple `Literal` values that are themselves not valid
	/// UTF-8, they might add up to valid UTF-8. This also demonstrates just
	/// how "smart" Hir's smart constructors are.
	///
	/// ```
	/// use regex_syntax::hir::{Hir, HirKind, Literal};
	///
	/// let literals = vec![
	///     Hir::literal([0xE2]),
	///     Hir::literal([0x98]),
	///     Hir::literal([0x83]),
	/// ];
	/// // Each literal, on its own, is invalid UTF-8.
	/// assert!(literals.iter().all(|hir| !hir.properties().is_utf8()));
	///
	/// let concat = Hir::concat(literals);
	/// // But the concatenation is valid UTF-8!
	/// assert!(concat.properties().is_utf8());
	///
	/// // And also notice that the literals have been concatenated into a
	/// // single `Literal`, to the point where there is no explicit `Concat`!
	/// let expected = HirKind::Literal(Literal(Box::from("☃".as_bytes())));
	/// assert_eq!(&expected, concat.kind());
	/// ```
	///
	/// # Example: building a literal from a `char`
	///
	/// This example shows how to build a single `Hir` literal from a `char`
	/// value. Since a [`Literal`] is just bytes, we just need to UTF-8
	/// encode a `char` value:
	///
	/// ```
	/// use regex_syntax::hir::{Hir, HirKind, Literal};
	///
	/// let ch = '☃';
	/// let got = Hir::literal(ch.encode_utf8(&mut [0; 4]).as_bytes());
	///
	/// let expected = HirKind::Literal(Literal(Box::from("☃".as_bytes())));
	/// assert_eq!(&expected, got.kind());
	/// ```
	#[inline]
	pub fn literal<B: Into<Box<[u8]>>>(lit: B) -> Hir {
		let bytes = lit.into();
		if bytes.is_empty() {
			return Hir::empty();
		}

		let lit = Literal(bytes);
		let props = Properties::literal(&lit);
		Hir { kind: HirKind::Literal(lit), props }
	}

	/// Creates a class HIR expression. The class may either be defined over
	/// ranges of Unicode codepoints or ranges of raw byte values.
	///
	/// Note that an empty class is permitted. An empty class is equivalent to
	/// `Hir::fail()`.
	#[inline]
	pub fn class(class: Class) -> Hir {
		if class.is_empty() {
			return Hir::fail();
		} else if let Some(bytes) = class.literal() {
			return Hir::literal(bytes);
		}
		let props = Properties::class(&class);
		Hir { kind: HirKind::Class(class), props }
	}

	/// Creates a look-around assertion HIR expression.
	#[inline]
	pub fn look(look: Look) -> Hir {
		let props = Properties::look(look);
		Hir { kind: HirKind::Look(look), props }
	}

	/// Creates a repetition HIR expression.
	#[inline]
	pub fn repetition(mut rep: Repetition) -> Hir {
		// If the sub-expression of a repetition can only match the empty
		// string, then we force its maximum to be at most 1.
		if rep.sub.properties().maximum_len() == Some(0) {
			rep.min = cmp::min(rep.min, 1);
			rep.max = rep.max.map(|n| cmp::min(n, 1)).or(Some(1));
		}
		// The regex 'a{0}' is always equivalent to the empty regex. This is
		// true even when 'a' is an expression that never matches anything
		// (like '\P{any}').
		//
		// Additionally, the regex 'a{1}' is always equivalent to 'a'.
		if rep.min == 0 && rep.max == Some(0) {
			return Hir::empty();
		} else if rep.min == 1 && rep.max == Some(1) {
			return *rep.sub;
		}
		let props = Properties::repetition(&rep);
		Hir { kind: HirKind::Repetition(rep), props }
	}

	/// Creates a capture HIR expression.
	///
	/// Note that there is no explicit HIR value for a non-capturing group.
	/// Since a non-capturing group only exists to override precedence in the
	/// concrete syntax and since an HIR already does its own grouping based on
	/// what is parsed, there is no need to explicitly represent non-capturing
	/// groups in the HIR.
	#[inline]
	pub fn capture(capture: Capture) -> Hir {
		let props = Properties::capture(&capture);
		Hir { kind: HirKind::Capture(capture), props }
	}

	/// Returns the concatenation of the given expressions.
	///
	/// This attempts to flatten and simplify the concatenation as appropriate.
	///
	/// # Example
	///
	/// This shows a simple example of basic flattening of both concatenations
	/// and literals.
	///
	/// ```
	/// use regex_syntax::hir::Hir;
	///
	/// let hir = Hir::concat(vec![
	///     Hir::concat(vec![
	///         Hir::literal([b'a']),
	///         Hir::literal([b'b']),
	///         Hir::literal([b'c']),
	///     ]),
	///     Hir::concat(vec![
	///         Hir::literal([b'x']),
	///         Hir::literal([b'y']),
	///         Hir::literal([b'z']),
	///     ]),
	/// ]);
	/// let expected = Hir::literal("abcxyz".as_bytes());
	/// assert_eq!(expected, hir);
	/// ```
	pub fn concat(subs: Vec<Hir>) -> Hir {
		// We rebuild the concatenation by simplifying it. Would be nice to do
		// it in place, but that seems a little tricky?
		let mut new = vec![];
		// This gobbles up any adjacent literals in a concatenation and smushes
		// them together. Basically, when we see a literal, we add its bytes
		// to 'prior_lit', and whenever we see anything else, we first take
		// any bytes in 'prior_lit' and add it to the 'new' concatenation.
		let mut prior_lit: Option<Vec<u8>> = None;
		for sub in subs {
			let (kind, props) = sub.into_parts();
			match kind {
				HirKind::Literal(Literal(bytes)) => {
					if let Some(ref mut prior_bytes) = prior_lit {
						prior_bytes.extend_from_slice(&bytes);
					} else {
						prior_lit = Some(bytes.to_vec());
					}
				}
				// We also flatten concats that are direct children of another
				// concat. We only need to do this one level deep since
				// Hir::concat is the only way to build concatenations, and so
				// flattening happens inductively.
				HirKind::Concat(subs2) => {
					for sub2 in subs2 {
						let (kind2, props2) = sub2.into_parts();
						match kind2 {
							HirKind::Literal(Literal(bytes)) => {
								if let Some(ref mut prior_bytes) = prior_lit {
									prior_bytes.extend_from_slice(&bytes);
								} else {
									prior_lit = Some(bytes.to_vec());
								}
							}
							kind2 => {
								if let Some(prior_bytes) = prior_lit.take() {
									new.push(Hir::literal(prior_bytes));
								}
								new.push(Hir { kind: kind2, props: props2 });
							}
						}
					}
				}
				// We can just skip empty HIRs.
				HirKind::Empty => {}
				kind => {
					if let Some(prior_bytes) = prior_lit.take() {
						new.push(Hir::literal(prior_bytes));
					}
					new.push(Hir { kind, props });
				}
			}
		}
		if let Some(prior_bytes) = prior_lit.take() {
			new.push(Hir::literal(prior_bytes));
		}
		if new.is_empty() {
			return Hir::empty();
		} else if new.len() == 1 {
			return new.pop().unwrap();
		}
		let props = Properties::concat(&new);
		Hir { kind: HirKind::Concat(new), props }
	}

	/// Returns the alternation of the given expressions.
	///
	/// This flattens and simplifies the alternation as appropriate. This may
	/// include factoring out common prefixes or even rewriting the alternation
	/// as a character class.
	///
	/// Note that an empty alternation is equivalent to `Hir::fail()`. (It
	/// is not possible for one to write an empty alternation, or even an
	/// alternation with a single sub-expression, in the concrete syntax of a
	/// regex.)
	///
	/// # Example
	///
	/// This is a simple example showing how an alternation might get
	/// simplified.
	///
	/// ```
	/// use regex_syntax::hir::{Hir, Class, ClassUnicode, ClassUnicodeRange};
	///
	/// let hir = Hir::alternation(vec![
	///     Hir::literal([b'a']),
	///     Hir::literal([b'b']),
	///     Hir::literal([b'c']),
	///     Hir::literal([b'd']),
	///     Hir::literal([b'e']),
	///     Hir::literal([b'f']),
	/// ]);
	/// let expected = Hir::class(Class::Unicode(ClassUnicode::new([
	///     ClassUnicodeRange::new('a', 'f'),
	/// ])));
	/// assert_eq!(expected, hir);
	/// ```
	///
	/// And another example showing how common prefixes might get factored
	/// out.
	///
	/// ```
	/// use regex_syntax::hir::{Hir, Class, ClassUnicode, ClassUnicodeRange};
	///
	/// let hir = Hir::alternation(vec![
	///     Hir::concat(vec![
	///         Hir::literal("abc".as_bytes()),
	///         Hir::class(Class::Unicode(ClassUnicode::new([
	///             ClassUnicodeRange::new('A', 'Z'),
	///         ]))),
	///     ]),
	///     Hir::concat(vec![
	///         Hir::literal("abc".as_bytes()),
	///         Hir::class(Class::Unicode(ClassUnicode::new([
	///             ClassUnicodeRange::new('a', 'z'),
	///         ]))),
	///     ]),
	/// ]);
	/// let expected = Hir::concat(vec![
	///     Hir::literal("abc".as_bytes()),
	///     Hir::alternation(vec![
	///         Hir::class(Class::Unicode(ClassUnicode::new([
	///             ClassUnicodeRange::new('A', 'Z'),
	///         ]))),
	///         Hir::class(Class::Unicode(ClassUnicode::new([
	///             ClassUnicodeRange::new('a', 'z'),
	///         ]))),
	///     ]),
	/// ]);
	/// assert_eq!(expected, hir);
	/// ```
	///
	/// Note that these sorts of simplifications are not guaranteed.
	pub fn alternation(subs: Vec<Hir>) -> Hir {
		// We rebuild the alternation by simplifying it. We proceed similarly
		// as the concatenation case. But in this case, there's no literal
		// simplification happening. We're just flattening alternations.
		let mut new = Vec::with_capacity(subs.len());
		for sub in subs {
			let (kind, props) = sub.into_parts();
			match kind {
				HirKind::Alternation(subs2) => {
					new.extend(subs2);
				}
				kind => {
					new.push(Hir { kind, props });
				}
			}
		}
		if new.is_empty() {
			return Hir::fail();
		} else if new.len() == 1 {
			return new.pop().unwrap();
		}
		// Now that it's completely flattened, look for the special case of
		// 'char1|char2|...|charN' and collapse that into a class. Note that
		// we look for 'char' first and then bytes. The issue here is that if
		// we find both non-ASCII codepoints and non-ASCII singleton bytes,
		// then it isn't actually possible to smush them into a single class.
		// (Because classes are either "all codepoints" or "all bytes." You
		// can have a class that both matches non-ASCII but valid UTF-8 and
		// invalid UTF-8.) So we look for all chars and then all bytes, and
		// don't handle anything else.
		if let Some(singletons) = singleton_chars(&new) {
			let it = singletons
				.into_iter()
				.map(|ch| ClassUnicodeRange { start: ch, end: ch });
			return Hir::class(Class::Unicode(ClassUnicode::new(it)));
		}
		if let Some(singletons) = singleton_bytes(&new) {
			let it = singletons
				.into_iter()
				.map(|b| ClassBytesRange { start: b, end: b });
			return Hir::class(Class::Bytes(ClassBytes::new(it)));
		}
		// Similar to singleton chars, we can also look for alternations of
		// classes. Those can be smushed into a single class.
		if let Some(cls) = class_chars(&new) {
			return Hir::class(cls);
		}
		if let Some(cls) = class_bytes(&new) {
			return Hir::class(cls);
		}
		// Factor out a common prefix if we can, which might potentially
		// simplify the expression and unlock other optimizations downstream.
		// It also might generally make NFA matching and DFA construction
		// faster by reducing the scope of branching in the regex.
		new = match lift_common_prefix(new) {
			Ok(hir) => return hir,
			Err(unchanged) => unchanged,
		};
		let props = Properties::alternation(&new);
		Hir { kind: HirKind::Alternation(new), props }
	}

	/// Returns an HIR expression for `.`.
	///
	/// * [`Dot::AnyChar`] maps to `(?su-R:.)`.
	/// * [`Dot::AnyByte`] maps to `(?s-Ru:.)`.
	/// * [`Dot::AnyCharExceptLF`] maps to `(?u-Rs:.)`.
	/// * [`Dot::AnyCharExceptCRLF`] maps to `(?Ru-s:.)`.
	/// * [`Dot::AnyByteExceptLF`] maps to `(?-Rsu:.)`.
	/// * [`Dot::AnyByteExceptCRLF`] maps to `(?R-su:.)`.
	///
	/// # Example
	///
	/// Note that this is a convenience routine for constructing the correct
	/// character class based on the value of `Dot`. There is no explicit "dot"
	/// HIR value. It is just an abbreviation for a common character class.
	///
	/// ```
	/// use regex_syntax::hir::{Hir, Dot, Class, ClassBytes, ClassBytesRange};
	///
	/// let hir = Hir::dot(Dot::AnyByte);
	/// let expected = Hir::class(Class::Bytes(ClassBytes::new([
	///     ClassBytesRange::new(0x00, 0xFF),
	/// ])));
	/// assert_eq!(expected, hir);
	/// ```
	#[inline]
	pub fn dot(dot: Dot) -> Hir {
		match dot {
			Dot::AnyChar => Hir::class(Class::Unicode(ClassUnicode::new([
				ClassUnicodeRange::new('\0', '\u{10FFFF}'),
			]))),
			Dot::AnyByte => Hir::class(Class::Bytes(ClassBytes::new([
				ClassBytesRange::new(b'\0', b'\xFF'),
			]))),
			Dot::AnyCharExcept(ch) => {
				let mut cls =
					ClassUnicode::new([ClassUnicodeRange::new(ch, ch)]);
				cls.negate();
				Hir::class(Class::Unicode(cls))
			}
			Dot::AnyCharExceptLF => {
				Hir::class(Class::Unicode(ClassUnicode::new([
					ClassUnicodeRange::new('\0', '\x09'),
					ClassUnicodeRange::new('\x0B', '\u{10FFFF}'),
				])))
			}
			Dot::AnyCharExceptCRLF => {
				Hir::class(Class::Unicode(ClassUnicode::new([
					ClassUnicodeRange::new('\0', '\x09'),
					ClassUnicodeRange::new('\x0B', '\x0C'),
					ClassUnicodeRange::new('\x0E', '\u{10FFFF}'),
				])))
			}
			Dot::AnyByteExcept(byte) => {
				let mut cls =
					ClassBytes::new([ClassBytesRange::new(byte, byte)]);
				cls.negate();
				Hir::class(Class::Bytes(cls))
			}
			Dot::AnyByteExceptLF => {
				Hir::class(Class::Bytes(ClassBytes::new([
					ClassBytesRange::new(b'\0', b'\x09'),
					ClassBytesRange::new(b'\x0B', b'\xFF'),
				])))
			}
			Dot::AnyByteExceptCRLF => {
				Hir::class(Class::Bytes(ClassBytes::new([
					ClassBytesRange::new(b'\0', b'\x09'),
					ClassBytesRange::new(b'\x0B', b'\x0C'),
					ClassBytesRange::new(b'\x0E', b'\xFF'),
				])))
			}
		}
	}
	 */
}

/// The underlying kind of an arbitrary [`Hir`] expression.
///
/// An `HirKind` is principally useful for doing case analysis on the type
/// of a regular expression. If you're looking to build new `Hir` values,
/// then you _must_ use the smart constructors defined on `Hir`, like
/// [`Hir::repetition`], to build new `Hir` values. The API intentionally does
/// not expose any way of building an `Hir` directly from an `HirKind`.
public indirect enum HirKind: Hashable, Sendable {
	/// The empty regular expression, which matches everything, including the
	/// empty string.
	case empty
	/// A literal string that matches exactly these bytes.
	case literal([UInt8])
	/// A single character class that matches any of the characters in the
	/// class. A class can either consist of Unicode scalar values as
	/// characters, or it can use bytes.
	///
	/// A class may be empty. In which case, it matches nothing.
	case `class`(CharacterClass)
	/// A look-around assertion. A look-around match always has zero length.
	case lookahead(LookaheadSet)
	/// A repetition operation applied to a sub-expression.
	case repetition(Repetition)
	/// A capturing group, which contains a sub-expression.
	case capture(Capture)
	/// A concatenation of expressions.
	///
	/// A concatenation matches only if each of its sub-expressions match one
	/// after the other.
	///
	/// Concatenations are guaranteed by `Hir`'s smart constructors to always
	/// have at least two sub-expressions.
	case concat([Hir])
	/// An alternation of expressions.
	///
	/// An alternation matches only if at least one of its sub-expressions
	/// match. If multiple sub-expressions match, then the leftmost is
	/// preferred.
	///
	/// Alternations are guaranteed by `Hir`'s smart constructors to always
	/// have at least two sub-expressions.
	case alternation([Hir])
}

public enum CharacterClass: Hashable, Sendable {
	case bytes(RangeSet<UInt8>)
	case unicode(RangeSet<UInt32>)
}

public struct LookaheadSet: Hashable, OptionSet, Sendable {
	public let rawValue: UInt32

	public init(rawValue: UInt32) {
		self.rawValue = rawValue
	}
}

extension LookaheadSet {
	/// Match the beginning of text.
	///
	/// Specifically, this matches at the starting position of the input.
	static let start = Self(rawValue: 0x1)
	/// Match the end of text.
	///
	/// Specifically, this matches at the ending position of the input.
	static let end = Self(rawValue: 0x2)
	/// Match the beginning of a line or the beginning of text.
	///
	/// Specifically, this matches at the starting position of the input, or at the position immediately following a `\n` character.
	static let startLF = Self(rawValue: 0x4)
	/// Match the end of a line or the end of text.
	///
	/// Specifically, this matches at the end position of the input, or at the position immediately preceding a `\n` character.
	static let endLF = Self(rawValue: 0x8)
	/// Match the beginning of a line or the beginning of text.
	///
	/// Specifically, this matches at the starting position of the input, or at the position immediately following either a `\r` or `\n` character, but never after a `\r` when a `\n` follows.
	static let StartCRLF = Self(rawValue: 0x10)
	/// Match the end of a line or the end of text.
	///
	/// Specifically, this matches at the end position of the input, or at the position immediately preceding a `\r` or `\n` character, but never before a `\n` when a `\r` precedes it.
	static let EndCRLF = Self(rawValue: 0x20)
	/// Match an ASCII-only word boundary.
	///
	/// That is, this matches a position where the left adjacent character and right adjacent character correspond to a word and non-word or a non-word and word character.
	static let WordAscii = Self(rawValue: 0x40)
	/// Match an ASCII-only negation of a word boundary.
	static let WordAsciiNegate = Self(rawValue: 0x80)
	/// Match a Unicode-aware word boundary.
	///
	/// That is, this matches a position where the left adjacent character and right adjacent character correspond to a word and non-word or a non-word and word character.
	static let WordUnicode = Self(rawValue: 0x100)
	/// Match a Unicode-aware negation of a word boundary.
	static let WordUnicodeNegate = Self(rawValue: 0x200)
	/// Match the start of an ASCII-only word boundary.
	///
	/// That is, this matches a position at either the beginning of the haystack or where the previous character is not a word character and the following character is a word character.
	static let WordStartAscii = Self(rawValue: 0x400)
	/// Match the end of an ASCII-only word boundary.
	///
	/// That is, this matches a position at either the end of the haystack or where the previous character is a word character and the following character is not a word character.
	static let WordEndAscii = Self(rawValue: 0x800)
	/// Match the start of a Unicode word boundary.
	///
	/// That is, this matches a position at either the beginning of the haystack or where the previous character is not a word character and the following character is a word character.
	static let WordStartUnicode = Self(rawValue: 0x1000)
	/// Match the end of a Unicode word boundary.
	///
	/// That is, this matches a position at either the end of the haystack or where the previous character is a word character and the following character is not a word character.
	static let WordEndUnicode = Self(rawValue: 0x2000)
	/// Match the start half of an ASCII-only word boundary.
	///
	/// That is, this matches a position at either the beginning of the haystack or where the previous character is not a word character.
	static let WordStartHalfAscii = Self(rawValue: 0x4000)
	/// Match the end half of an ASCII-only word boundary.
	///
	/// That is, this matches a position at either the end of the haystack or where the following character is not a word character.
	static let WordEndHalfAscii = Self(rawValue: 0x8000)
	/// Match the start half of a Unicode word boundary.
	///
	/// That is, this matches a position at either the beginning of the haystack or where the previous character is not a word character.
	static let WordStartHalfUnicode = Self(rawValue: 0x10000)
	/// Match the end half of a Unicode word boundary.
	///
	/// That is, this matches a position at either the end of the haystack or where the following character is not a word character.
	static let WordEndHalfUnicode = Self(rawValue: 0x20000)
}

/// The high-level intermediate representation of a repetition operator.
///
/// A repetition operator permits the repetition of an arbitrary sub-expression.
public struct Repetition: Hashable, Sendable {
	/// The minimum range of the repetition.
	///
	/// Note that special cases like `?`, `+` and `*` all get translated into
	/// the ranges `{0,1}`, `{1,}` and `{0,}`, respectively.
	///
	/// When `min` is zero, this expression can match the empty string
	/// regardless of what its sub-expression is.
	public var min: UInt32
	/// The maximum range of the repetition.
	///
	/// Note that when `max` is `nil`, `min` acts as a lower bound but where
	/// there is no upper bound. For something like `x{5}` where the min and
	/// max are equivalent, `min` will be set to `5` and `max` will be set to
	/// `5`.
	public var max: UInt32?
	/// Whether this repetition operator is greedy or not. A greedy operator
	/// will match as much as it can. A non-greedy operator will match as
	/// little as it can.
	///
	/// Typically, operators are greedy by default and are only non-greedy when
	/// a `?` suffix is used, e.g., `(expr)*` is greedy while `(expr)*?` is
	/// not. However, this can be inverted via the `U` "ungreedy" flag.
	public var isGreedy = false
	/// The expression being repeated.
	public var sub: Hir
}

/// The high-level intermediate representation for a capturing group.
///
/// A capturing group always has an index and a child expression. It may
/// also have a name associated with it (e.g., `(?P<foo>\w)`), but it's not
/// necessary.
///
/// Note that there is no explicit representation of a non-capturing group
/// in a `Hir`. Instead, non-capturing grouping is handled automatically by
/// the recursive structure of the `Hir` itself.
public struct Capture: Hashable, Sendable {
	/// The capture index of the capture.
	public var index: UInt32
	/// The name of the capture, if it exists.
	public var name: Substring?
	/// The expression inside the capturing group, which may be empty.
	public var sub: Hir
}

/// A type that collects various properties of an HIR value.
///
/// Properties are always scalar values and represent meta data that is
/// computed inductively on an HIR value. Properties are defined for all
/// HIR values.
///
/// All methods on a `Properties` value take constant time and are meant to
/// be cheap to call.
public struct Properties: Hashable, Sendable {
	//FIXME: Box, requires ~Copyable, non of the stdlib protocols support it
	var box: PropertiesI
}

/// The property definition. It is split out so that we can box it, and
/// there by make `Properties` use less stack size. This is kind-of important
/// because every HIR value has a `Properties` attached to it.
///
/// This does have the unfortunate consequence that creating any HIR value
/// always leads to at least one alloc for properties, but this is generally
/// true anyway (for pretty much all HirKinds except for look-arounds).
struct PropertiesI: Hashable, Sendable {
	var minimum_len: UInt?
	var maximum_len: UInt?
	var look_set: LookaheadSet = []
	var look_set_prefix: LookaheadSet = []
	var look_set_suffix: LookaheadSet = []
	var look_set_prefix_any: LookaheadSet = []
	var look_set_suffix_any: LookaheadSet = []
	// It is debatable whether an empty regex always matches at valid
	// UTF-8 boundaries. Strictly speaking, at a byte oriented view,
	// it is clearly false. There are, for example, many empty strings
	// between the bytes encoding a '☃'.
	//
	// However, when Unicode mode is enabled, the fundamental atom
	// of matching is really a codepoint. And in that scenario, an
	// empty regex is defined to only match at valid UTF-8 boundaries
	// and to never split a codepoint. It just so happens that this
	// enforcement is somewhat tricky to do for regexes that match
	// the empty string inside regex engines themselves. It usually
	// requires some layer above the regex engine to filter out such
	// matches.
	//
	// In any case, 'true' is really the only coherent option. If it
	// were false, for example, then 'a*' would also need to be false
	// since it too can match the empty string.
	var isUtf8 = true
	var explicit_captures_len: UInt = 0
	var static_explicit_captures_len: UInt?
	var isLiteral = false
	var isAlternationLiteral = false
}

public extension Properties {
	 /*
	/// Returns the length (in bytes) of the smallest string matched by this
	/// HIR.
	///
	/// A return value of `0` is possible and occurs when the HIR can match an
	/// empty string.
	///
	/// `None` is returned when there is no minimum length. This occurs in
	/// precisely the cases where the HIR matches nothing. i.e., The language
	/// the regex matches is empty. An example of such a regex is `\P{any}`.
	#[inline]
	pub fn minimum_len(&self) -> Option<usize> {
		self.0.minimum_len
	}

	/// Returns the length (in bytes) of the longest string matched by this
	/// HIR.
	///
	/// A return value of `0` is possible and occurs when nothing longer than
	/// the empty string is in the language described by this HIR.
	///
	/// `None` is returned when there is no longest matching string. This
	/// occurs when the HIR matches nothing or when there is no upper bound on
	/// the length of matching strings. Example of such regexes are `\P{any}`
	/// (matches nothing) and `a+` (has no upper bound).
	#[inline]
	pub fn maximum_len(&self) -> Option<usize> {
		self.0.maximum_len
	}

	/// Returns a set of all look-around assertions that appear at least once
	/// in this HIR value.
	#[inline]
	pub fn look_set(&self) -> LookSet {
		self.0.look_set
	}

	/// Returns a set of all look-around assertions that appear as a prefix for
	/// this HIR value. That is, the set returned corresponds to the set of
	/// assertions that must be passed before matching any bytes in a haystack.
	///
	/// For example, `hir.look_set_prefix().contains(Look::Start)` returns true
	/// if and only if the HIR is fully anchored at the start.
	#[inline]
	pub fn look_set_prefix(&self) -> LookSet {
		self.0.look_set_prefix
	}

	/// Returns a set of all look-around assertions that appear as a _possible_
	/// prefix for this HIR value. That is, the set returned corresponds to the
	/// set of assertions that _may_ be passed before matching any bytes in a
	/// haystack.
	///
	/// For example, `hir.look_set_prefix_any().contains(Look::Start)` returns
	/// true if and only if it's possible for the regex to match through a
	/// anchored assertion before consuming any input.
	#[inline]
	pub fn look_set_prefix_any(&self) -> LookSet {
		self.0.look_set_prefix_any
	}

	/// Returns a set of all look-around assertions that appear as a suffix for
	/// this HIR value. That is, the set returned corresponds to the set of
	/// assertions that must be passed in order to be considered a match after
	/// all other consuming HIR expressions.
	///
	/// For example, `hir.look_set_suffix().contains(Look::End)` returns true
	/// if and only if the HIR is fully anchored at the end.
	#[inline]
	pub fn look_set_suffix(&self) -> LookSet {
		self.0.look_set_suffix
	}

	/// Returns a set of all look-around assertions that appear as a _possible_
	/// suffix for this HIR value. That is, the set returned corresponds to the
	/// set of assertions that _may_ be passed before matching any bytes in a
	/// haystack.
	///
	/// For example, `hir.look_set_suffix_any().contains(Look::End)` returns
	/// true if and only if it's possible for the regex to match through a
	/// anchored assertion at the end of a match without consuming any input.
	#[inline]
	pub fn look_set_suffix_any(&self) -> LookSet {
		self.0.look_set_suffix_any
	}

	/// Return true if and only if the corresponding HIR will always match
	/// valid UTF-8.
	///
	/// When this returns false, then it is possible for this HIR expression to
	/// match invalid UTF-8, including by matching between the code units of
	/// a single UTF-8 encoded codepoint.
	///
	/// Note that this returns true even when the corresponding HIR can match
	/// the empty string. Since an empty string can technically appear between
	/// UTF-8 code units, it is possible for a match to be reported that splits
	/// a codepoint which could in turn be considered matching invalid UTF-8.
	/// However, it is generally assumed that such empty matches are handled
	/// specially by the search routine if it is absolutely required that
	/// matches not split a codepoint.
	///
	/// # Example
	///
	/// This code example shows the UTF-8 property of a variety of patterns.
	///
	/// ```
	/// use regex_syntax::{ParserBuilder, parse};
	///
	/// // Examples of 'is_utf8() == true'.
	/// assert!(parse(r"a")?.properties().is_utf8());
	/// assert!(parse(r"[^a]")?.properties().is_utf8());
	/// assert!(parse(r".")?.properties().is_utf8());
	/// assert!(parse(r"\W")?.properties().is_utf8());
	/// assert!(parse(r"\b")?.properties().is_utf8());
	/// assert!(parse(r"\B")?.properties().is_utf8());
	/// assert!(parse(r"(?-u)\b")?.properties().is_utf8());
	/// assert!(parse(r"(?-u)\B")?.properties().is_utf8());
	/// // Unicode mode is enabled by default, and in
	/// // that mode, all \x hex escapes are treated as
	/// // codepoints. So this actually matches the UTF-8
	/// // encoding of U+00FF.
	/// assert!(parse(r"\xFF")?.properties().is_utf8());
	///
	/// // Now we show examples of 'is_utf8() == false'.
	/// // The only way to do this is to force the parser
	/// // to permit invalid UTF-8, otherwise all of these
	/// // would fail to parse!
	/// let parse = |pattern| {
	///     ParserBuilder::new().utf8(false).build().parse(pattern)
	/// };
	/// assert!(!parse(r"(?-u)[^a]")?.properties().is_utf8());
	/// assert!(!parse(r"(?-u).")?.properties().is_utf8());
	/// assert!(!parse(r"(?-u)\W")?.properties().is_utf8());
	/// // Conversely to the equivalent example above,
	/// // when Unicode mode is disabled, \x hex escapes
	/// // are treated as their raw byte values.
	/// assert!(!parse(r"(?-u)\xFF")?.properties().is_utf8());
	/// // Note that just because we disabled UTF-8 in the
	/// // parser doesn't mean we still can't use Unicode.
	/// // It is enabled by default, so \xFF is still
	/// // equivalent to matching the UTF-8 encoding of
	/// // U+00FF by default.
	/// assert!(parse(r"\xFF")?.properties().is_utf8());
	/// // Even though we use raw bytes that individually
	/// // are not valid UTF-8, when combined together, the
	/// // overall expression *does* match valid UTF-8!
	/// assert!(parse(r"(?-u)\xE2\x98\x83")?.properties().is_utf8());
	///
	/// # Ok::<(), Box<dyn std::error::Error>>(())
	/// ```
	#[inline]
	pub fn is_utf8(&self) -> bool {
		self.0.utf8
	}

	/// Returns the total number of explicit capturing groups in the
	/// corresponding HIR.
	///
	/// Note that this does not include the implicit capturing group
	/// corresponding to the entire match that is typically included by regex
	/// engines.
	///
	/// # Example
	///
	/// This method will return `0` for `a` and `1` for `(a)`:
	///
	/// ```
	/// use regex_syntax::parse;
	///
	/// assert_eq!(0, parse("a")?.properties().explicit_captures_len());
	/// assert_eq!(1, parse("(a)")?.properties().explicit_captures_len());
	///
	/// # Ok::<(), Box<dyn std::error::Error>>(())
	/// ```
	#[inline]
	pub fn explicit_captures_len(&self) -> usize {
		self.0.explicit_captures_len
	}

	/// Returns the total number of explicit capturing groups that appear in
	/// every possible match.
	///
	/// If the number of capture groups can vary depending on the match, then
	/// this returns `None`. That is, a value is only returned when the number
	/// of matching groups is invariant or "static."
	///
	/// Note that this does not include the implicit capturing group
	/// corresponding to the entire match.
	///
	/// # Example
	///
	/// This shows a few cases where a static number of capture groups is
	/// available and a few cases where it is not.
	///
	/// ```
	/// use regex_syntax::parse;
	///
	/// let len = |pattern| {
	///     parse(pattern).map(|h| {
	///         h.properties().static_explicit_captures_len()
	///     })
	/// };
	///
	/// assert_eq!(Some(0), len("a")?);
	/// assert_eq!(Some(1), len("(a)")?);
	/// assert_eq!(Some(1), len("(a)|(b)")?);
	/// assert_eq!(Some(2), len("(a)(b)|(c)(d)")?);
	/// assert_eq!(None, len("(a)|b")?);
	/// assert_eq!(None, len("a|(b)")?);
	/// assert_eq!(None, len("(b)*")?);
	/// assert_eq!(Some(1), len("(b)+")?);
	///
	/// # Ok::<(), Box<dyn std::error::Error>>(())
	/// ```
	#[inline]
	pub fn static_explicit_captures_len(&self) -> Option<usize> {
		self.0.static_explicit_captures_len
	}

	/// Return true if and only if this HIR is a simple literal. This is
	/// only true when this HIR expression is either itself a `Literal` or a
	/// concatenation of only `Literal`s.
	///
	/// For example, `f` and `foo` are literals, but `f+`, `(foo)`, `foo()` and
	/// the empty string are not (even though they contain sub-expressions that
	/// are literals).
	#[inline]
	pub fn is_literal(&self) -> bool {
		self.0.literal
	}

	/// Return true if and only if this HIR is either a simple literal or an
	/// alternation of simple literals. This is only
	/// true when this HIR expression is either itself a `Literal` or a
	/// concatenation of only `Literal`s or an alternation of only `Literal`s.
	///
	/// For example, `f`, `foo`, `a|b|c`, and `foo|bar|baz` are alternation
	/// literals, but `f+`, `(foo)`, `foo()`, and the empty pattern are not
	/// (even though that contain sub-expressions that are literals).
	#[inline]
	pub fn is_alternation_literal(&self) -> bool {
		self.0.alternation_literal
	}

	/// Returns the total amount of heap memory usage, in bytes, used by this
	/// `Properties` value.
	#[inline]
	pub fn memory_usage(&self) -> usize {
		core::mem::size_of::<PropertiesI>()
	}

	/// Returns a new set of properties that corresponds to the union of the
	/// iterator of properties given.
	///
	/// This is useful when one has multiple `Hir` expressions and wants
	/// to combine them into a single alternation without constructing the
	/// corresponding `Hir`. This routine provides a way of combining the
	/// properties of each `Hir` expression into one set of properties
	/// representing the union of those expressions.
	///
	/// # Example: union with HIRs that never match
	///
	/// This example shows that unioning properties together with one that
	/// represents a regex that never matches will "poison" certain attributes,
	/// like the minimum and maximum lengths.
	///
	/// ```
	/// use regex_syntax::{hir::Properties, parse};
	///
	/// let hir1 = parse("ab?c?")?;
	/// assert_eq!(Some(1), hir1.properties().minimum_len());
	/// assert_eq!(Some(3), hir1.properties().maximum_len());
	///
	/// let hir2 = parse(r"[a&&b]")?;
	/// assert_eq!(None, hir2.properties().minimum_len());
	/// assert_eq!(None, hir2.properties().maximum_len());
	///
	/// let hir3 = parse(r"wxy?z?")?;
	/// assert_eq!(Some(2), hir3.properties().minimum_len());
	/// assert_eq!(Some(4), hir3.properties().maximum_len());
	///
	/// let unioned = Properties::union([
	///		hir1.properties(),
	///		hir2.properties(),
	///		hir3.properties(),
	///	]);
	/// assert_eq!(None, unioned.minimum_len());
	/// assert_eq!(None, unioned.maximum_len());
	///
	/// # Ok::<(), Box<dyn std::error::Error>>(())
	/// ```
	///
	/// The maximum length can also be "poisoned" by a pattern that has no
	/// upper bound on the length of a match. The minimum length remains
	/// unaffected:
	///
	/// ```
	/// use regex_syntax::{hir::Properties, parse};
	///
	/// let hir1 = parse("ab?c?")?;
	/// assert_eq!(Some(1), hir1.properties().minimum_len());
	/// assert_eq!(Some(3), hir1.properties().maximum_len());
	///
	/// let hir2 = parse(r"a+")?;
	/// assert_eq!(Some(1), hir2.properties().minimum_len());
	/// assert_eq!(None, hir2.properties().maximum_len());
	///
	/// let hir3 = parse(r"wxy?z?")?;
	/// assert_eq!(Some(2), hir3.properties().minimum_len());
	/// assert_eq!(Some(4), hir3.properties().maximum_len());
	///
	/// let unioned = Properties::union([
	///		hir1.properties(),
	///		hir2.properties(),
	///		hir3.properties(),
	///	]);
	/// assert_eq!(Some(1), unioned.minimum_len());
	/// assert_eq!(None, unioned.maximum_len());
	///
	/// # Ok::<(), Box<dyn std::error::Error>>(())
	/// ```
	pub fn union<I, P>(props: I) -> Properties
	where
		I: IntoIterator<Item = P>,
		P: core::borrow::Borrow<Properties>,
	{
		let mut it = props.into_iter().peekable();
		// While empty alternations aren't possible, we still behave as if they
		// are. When we have an empty alternate, then clearly the look-around
		// prefix and suffix is empty. Otherwise, it is the intersection of all
		// prefixes and suffixes (respectively) of the branches.
		let fix = if it.peek().is_none() {
			LookSet::empty()
		} else {
			LookSet::full()
		};
		// And also, an empty alternate means we have 0 static capture groups,
		// but we otherwise start with the number corresponding to the first
		// alternate. If any subsequent alternate has a different number of
		// static capture groups, then we overall have a variation and not a
		// static number of groups.
		let static_explicit_captures_len =
			it.peek().and_then(|p| p.borrow().static_explicit_captures_len());
		// The base case is an empty alternation, which matches nothing.
		// Note though that empty alternations aren't possible, because the
		// Hir::alternation smart constructor rewrites those as empty character
		// classes.
		let mut props = PropertiesI {
			minimum_len: None,
			maximum_len: None,
			look_set: LookSet::empty(),
			look_set_prefix: fix,
			look_set_suffix: fix,
			look_set_prefix_any: LookSet::empty(),
			look_set_suffix_any: LookSet::empty(),
			utf8: true,
			explicit_captures_len: 0,
			static_explicit_captures_len,
			literal: false,
			alternation_literal: true,
		};
		let (mut min_poisoned, mut max_poisoned) = (false, false);
		// Handle properties that need to visit every child hir.
		for prop in it {
			let p = prop.borrow();
			props.look_set.set_union(p.look_set());
			props.look_set_prefix.set_intersect(p.look_set_prefix());
			props.look_set_suffix.set_intersect(p.look_set_suffix());
			props.look_set_prefix_any.set_union(p.look_set_prefix_any());
			props.look_set_suffix_any.set_union(p.look_set_suffix_any());
			props.utf8 = props.utf8 && p.is_utf8();
			props.explicit_captures_len = props
				.explicit_captures_len
				.saturating_add(p.explicit_captures_len());
			if props.static_explicit_captures_len
				!= p.static_explicit_captures_len()
			{
				props.static_explicit_captures_len = None;
			}
			props.alternation_literal =
				props.alternation_literal && p.is_literal();
			if !min_poisoned {
				if let Some(xmin) = p.minimum_len() {
					if props.minimum_len.map_or(true, |pmin| xmin < pmin) {
						props.minimum_len = Some(xmin);
					}
				} else {
					props.minimum_len = None;
					min_poisoned = true;
				}
			}
			if !max_poisoned {
				if let Some(xmax) = p.maximum_len() {
					if props.maximum_len.map_or(true, |pmax| xmax > pmax) {
						props.maximum_len = Some(xmax);
					}
				} else {
					props.maximum_len = None;
					max_poisoned = true;
				}
			}
		}
		Properties(Box::new(props))
	}
	 */

	/// Create a new set of HIR properties for an empty regex.
	static var empty: Properties {
		Properties(box: PropertiesI())
	}

/*
	/// Create a new set of HIR properties for a literal regex.
	fn literal(lit: &Literal) -> Properties {
		let inner = PropertiesI {
			minimum_len: Some(lit.0.len()),
			maximum_len: Some(lit.0.len()),
			look_set: LookSet::empty(),
			look_set_prefix: LookSet::empty(),
			look_set_suffix: LookSet::empty(),
			look_set_prefix_any: LookSet::empty(),
			look_set_suffix_any: LookSet::empty(),
			utf8: core::str::from_utf8(&lit.0).is_ok(),
			explicit_captures_len: 0,
			static_explicit_captures_len: Some(0),
			literal: true,
			alternation_literal: true,
		};
		Properties(Box::new(inner))
	}

	/// Create a new set of HIR properties for a character class.
	fn class(class: &Class) -> Properties {
		let inner = PropertiesI {
			minimum_len: class.minimum_len(),
			maximum_len: class.maximum_len(),
			look_set: LookSet::empty(),
			look_set_prefix: LookSet::empty(),
			look_set_suffix: LookSet::empty(),
			look_set_prefix_any: LookSet::empty(),
			look_set_suffix_any: LookSet::empty(),
			utf8: class.is_utf8(),
			explicit_captures_len: 0,
			static_explicit_captures_len: Some(0),
			literal: false,
			alternation_literal: false,
		};
		Properties(Box::new(inner))
	}

	/// Create a new set of HIR properties for a look-around assertion.
	fn look(look: Look) -> Properties {
		let inner = PropertiesI {
			minimum_len: Some(0),
			maximum_len: Some(0),
			look_set: LookSet::singleton(look),
			look_set_prefix: LookSet::singleton(look),
			look_set_suffix: LookSet::singleton(look),
			look_set_prefix_any: LookSet::singleton(look),
			look_set_suffix_any: LookSet::singleton(look),
			// This requires a little explanation. Basically, we don't consider
			// matching an empty string to be equivalent to matching invalid
			// UTF-8, even though technically matching every empty string will
			// split the UTF-8 encoding of a single codepoint when treating a
			// UTF-8 encoded string as a sequence of bytes. Our defense here is
			// that in such a case, a codepoint should logically be treated as
			// the fundamental atom for matching, and thus the only valid match
			// points are between codepoints and not bytes.
			//
			// More practically, this is true here because it's also true
			// for 'Hir::empty()', otherwise something like 'a*' would be
			// considered to match invalid UTF-8. That in turn makes this
			// property borderline useless.
			utf8: true,
			explicit_captures_len: 0,
			static_explicit_captures_len: Some(0),
			literal: false,
			alternation_literal: false,
		};
		Properties(Box::new(inner))
	}

	/// Create a new set of HIR properties for a repetition.
	fn repetition(rep: &Repetition) -> Properties {
		let p = rep.sub.properties();
		let minimum_len = p.minimum_len().map(|child_min| {
			let rep_min = usize::try_from(rep.min).unwrap_or(usize::MAX);
			child_min.saturating_mul(rep_min)
		});
		let maximum_len = rep.max.and_then(|rep_max| {
			let rep_max = usize::try_from(rep_max).ok()?;
			let child_max = p.maximum_len()?;
			child_max.checked_mul(rep_max)
		});

		let mut inner = PropertiesI {
			minimum_len,
			maximum_len,
			look_set: p.look_set(),
			look_set_prefix: LookSet::empty(),
			look_set_suffix: LookSet::empty(),
			look_set_prefix_any: p.look_set_prefix_any(),
			look_set_suffix_any: p.look_set_suffix_any(),
			utf8: p.is_utf8(),
			explicit_captures_len: p.explicit_captures_len(),
			static_explicit_captures_len: p.static_explicit_captures_len(),
			literal: false,
			alternation_literal: false,
		};
		// If the repetition operator can match the empty string, then its
		// lookset prefix and suffixes themselves remain empty since they are
		// no longer required to match.
		if rep.min > 0 {
			inner.look_set_prefix = p.look_set_prefix();
			inner.look_set_suffix = p.look_set_suffix();
		}
		// If the static captures len of the sub-expression is not known or
		// is greater than zero, then it automatically propagates to the
		// repetition, regardless of the repetition. Otherwise, it might
		// change, but only when the repetition can match 0 times.
		if rep.min == 0
			&& inner.static_explicit_captures_len.map_or(false, |len| len > 0)
		{
			// If we require a match 0 times, then our captures len is
			// guaranteed to be zero. Otherwise, if we *can* match the empty
			// string, then it's impossible to know how many captures will be
			// in the resulting match.
			if rep.max == Some(0) {
				inner.static_explicit_captures_len = Some(0);
			} else {
				inner.static_explicit_captures_len = None;
			}
		}
		Properties(Box::new(inner))
	}

	/// Create a new set of HIR properties for a capture.
	fn capture(capture: &Capture) -> Properties {
		let p = capture.sub.properties();
		Properties(Box::new(PropertiesI {
			explicit_captures_len: p.explicit_captures_len().saturating_add(1),
			static_explicit_captures_len: p
				.static_explicit_captures_len()
				.map(|len| len.saturating_add(1)),
			literal: false,
			alternation_literal: false,
			..*p.0.clone()
		}))
	}

	/// Create a new set of HIR properties for a concatenation.
	fn concat(concat: &[Hir]) -> Properties {
		// The base case is an empty concatenation, which matches the empty
		// string. Note though that empty concatenations aren't possible,
		// because the Hir::concat smart constructor rewrites those as
		// Hir::empty.
		let mut props = PropertiesI {
			minimum_len: Some(0),
			maximum_len: Some(0),
			look_set: LookSet::empty(),
			look_set_prefix: LookSet::empty(),
			look_set_suffix: LookSet::empty(),
			look_set_prefix_any: LookSet::empty(),
			look_set_suffix_any: LookSet::empty(),
			utf8: true,
			explicit_captures_len: 0,
			static_explicit_captures_len: Some(0),
			literal: true,
			alternation_literal: true,
		};
		// Handle properties that need to visit every child hir.
		for x in concat.iter() {
			let p = x.properties();
			props.look_set.set_union(p.look_set());
			props.utf8 = props.utf8 && p.is_utf8();
			props.explicit_captures_len = props
				.explicit_captures_len
				.saturating_add(p.explicit_captures_len());
			props.static_explicit_captures_len = p
				.static_explicit_captures_len()
				.and_then(|len1| {
					Some((len1, props.static_explicit_captures_len?))
				})
				.and_then(|(len1, len2)| Some(len1.saturating_add(len2)));
			props.literal = props.literal && p.is_literal();
			props.alternation_literal =
				props.alternation_literal && p.is_alternation_literal();
			if let Some(minimum_len) = props.minimum_len {
				match p.minimum_len() {
					None => props.minimum_len = None,
					Some(len) => {
						// We use saturating arithmetic here because the
						// minimum is just a lower bound. We can't go any
						// higher than what our number types permit.
						props.minimum_len =
							Some(minimum_len.saturating_add(len));
					}
				}
			}
			if let Some(maximum_len) = props.maximum_len {
				match p.maximum_len() {
					None => props.maximum_len = None,
					Some(len) => {
						props.maximum_len = maximum_len.checked_add(len)
					}
				}
			}
		}
		// Handle the prefix properties, which only requires visiting
		// child exprs until one matches more than the empty string.
		let mut it = concat.iter();
		while let Some(x) = it.next() {
			props.look_set_prefix.set_union(x.properties().look_set_prefix());
			props
				.look_set_prefix_any
				.set_union(x.properties().look_set_prefix_any());
			if x.properties().maximum_len().map_or(true, |x| x > 0) {
				break;
			}
		}
		// Same thing for the suffix properties, but in reverse.
		let mut it = concat.iter().rev();
		while let Some(x) = it.next() {
			props.look_set_suffix.set_union(x.properties().look_set_suffix());
			props
				.look_set_suffix_any
				.set_union(x.properties().look_set_suffix_any());
			if x.properties().maximum_len().map_or(true, |x| x > 0) {
				break;
			}
		}
		Properties(Box::new(props))
	}

	/// Create a new set of HIR properties for a concatenation.
	fn alternation(alts: &[Hir]) -> Properties {
		Properties::union(alts.iter().map(|hir| hir.properties()))
	}
	 */
}
