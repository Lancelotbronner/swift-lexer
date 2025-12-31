////
////  Pattern.swift
////  swift-lexer
////
////  Created by Christophe Bronner on 2025-12-31.
////
//
//import RegexSyntax
//
//struct Pattern: CustomStringConvertible {
//	/// The original source literal for this pattern.
//	var source: String
//	/// The parsed regex HIR for this pattern.
//	var hir: Hir
//}
//
//extension Pattern {
//	/*
//	/// Create a new pattern from a regex string source.
//	/// - `utf8_mode` determines if the pattern should refuse to match invalid utf8 sequences
//	/// - `unicode` determines if the regex pattern should match bytes (false) or utf8 codepoints
//	///   (true)
//	/// - `ignore_case` sets the (?i) flag for the entire pattern.
//	///
//	/// There are some cases where the value of `utf8_mode` and `unicode` may not match. For
//	/// example, if your input is a `[u8]`, but you want to match specific parts of it as UTF-8, you
//	/// would have only `unicode` set.
//	pub fn compile(
//		is_literal: bool,
//		regex: &str,
//		source: String,
//		unicode: bool,
//		ignore_case: bool,
//	) -> Result<Pattern, String> {
//		// UTF-8 mode is disabled here so we can give prettier error messages
//		// later in the compilation process. See logos_codegen/src/lib.rs for
//		// the utf8 checking.
//		let hir = ParserBuilder::new()
//			.utf8(false)
//			.unicode(unicode)
//			.case_insensitive(ignore_case)
//			.build()
//			.parse(regex)
//			.map_err(|err| format!("{err}"))?;
//
//		Ok(Pattern {
//			is_literal,
//			source,
//			hir,
//		})
//	}
//
//	/// Create a pattern that matches a literal.
//	///
//	/// This function avoids escaping by constructing an Hir literal directly.
//	pub fn compile_lit(source: &Literal) -> Result<Pattern, String> {
//		let hir = match source {
//			Literal::Utf8(lit_str) => Hir::literal(lit_str.value().as_bytes()),
//			Literal::Bytes(lit_byte_str) => Hir::literal(lit_byte_str.value()),
//		};
//
//		Ok(Pattern {
//			is_literal: true,
//			source: source.token().to_string(),
//			hir,
//		})
//	}
//
//	/// Get the default priority for a pattern
//	pub fn priority(&self) -> usize {
//		Self::complexity(&self.hir)
//	}
//
//	fn complexity(hir: &Hir) -> usize {
//		match hir.kind() {
//			HirKind::Empty => 0,
//			HirKind::Literal(lit) => match std::str::from_utf8(&lit.0) {
//				Ok(s) => 2 * s.chars().count(),
//				Err(_) => 2 * lit.0.len(),
//			},
//			HirKind::Class(_) => 2,
//			HirKind::Look(_) => 0,
//			HirKind::Repetition(repetition) => {
//				repetition.min as usize * Self::complexity(&repetition.sub)
//			}
//			HirKind::Capture(capture) => Self::complexity(&capture.sub),
//			HirKind::Concat(hirs) => hirs.iter().map(Self::complexity).sum(),
//			HirKind::Alternation(hirs) => hirs.iter().map(Self::complexity).min().unwrap_or(0),
//		}
//	}
//	 */
//
//	/// Return true if this pattern contains a non-greedy `.+` or `.*`
//	func check_for_greedy_all() -> Bool {
//		hir.hasGreedyAll()
//	}
//
//	var description: String {
//		"@Token(\(source)"
//	}
//}
