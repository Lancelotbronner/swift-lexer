//
//  BenchTest.swift
//
//
//  Created by Larry Zeng on 12/4/23.
//

import Testing
import XCTest
import Lexing

@Lexer(skip: "\t| |\n")
enum BenchTestToken {
	@Token(/[a-zA-Z_$][a-zA-Z0-9_$]*?/)
	case Identifier

	@Token(/"([^"\\]|\\t|\\n|\\n|\\")*?"/)
	case String

	@Token(#"private"#)
	case Private

	@Token(#"primitive"#)
	case Primitive

	@Token(#"protected"#)
	case Protected

	@Token(#"in"#)
	case In

	@Token(#"instanceof"#)
	case Instanceof

	@Token(#"."#)
	case Accessor

	@Token(#"..."#)
	case Ellipsis

	@Token(#"("#)
	case ParenOpen

	@Token(#")"#)
	case ParenClose

	@Token(#"{"#)
	case BraceOpen

	@Token(#"}"#)
	case BraceClose

	@Token(#"+"#)
	case OpAddition

	@Token(#"++"#)
	case OpIncrement

	@Token(#"="#)
	case OpAssign

	@Token(#"=="#)
	case OpEquality

	@Token(#"==="#)
	case OpStrictEquality

	@Token(#"=>"#)
	case FatArrow
}

let SOURCE = """
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
foobar(protected primitive private instanceof in) { + ++ = == === => }
"""

let IDENTIFIERS = """
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton \
It was the year when they finally immanentized the Eschaton
"""

let STRINGS = #""tree" "to" "a" "graph" "that can" "more adequately represent" "loops and arbitrary state jumps" "with\"\"\"out" "the\n\n\n\n\n" "expl\"\"\"osive" "nature\"""of trying to build up all possible permutations in a tree." "tree" "to" "a" "graph" "that can" "more adequately represent" "loops and arbitrary state jumps" "with\"\"\"out" "the\n\n\n\n\n" "expl\"\"\"osive" "nature\"""of trying to build up all possible permutations in a tree." "tree" "to" "a" "graph" "that can" "more adequately represent" "loops and arbitrary state jumps" "with\"\"\"out" "the\n\n\n\n\n" "expl\"\"\"osive" "nature\"""of trying to build up all possible permutations in a tree." "tree" "to" "a" "graph" "that can" "more adequately represent" "loops and arbitrary state jumps" "with\"\"\"out" "the\n\n\n\n\n" "expl\"\"\"osive" "nature\"""of trying to build up all possible permutations in a tree.""#

let BENCHMARKS = [
	(30 * 16, SOURCE),
	(13 * 10, IDENTIFIERS),
	(48, STRINGS)
]

@Suite
struct BenchmarkTests {
	@Test(arguments: BENCHMARKS)
	func correct(num: Int, source: String) throws {
		let tokens = Array(BenchTestToken.lexer(source: source))

		for token in tokens {
			_ = try token.get()
		}

		#expect(tokens.count == num)
	}

	@Test(arguments: BENCHMARKS)
	func speed(num: Int, source: String) throws {
		let ITERATION_COUNT = 10
		let startTime = Date()
		for _ in 0 ..< ITERATION_COUNT {
			_ = Array(BenchTestToken.lexer(source: source).map { $0.get })
		}
		let endTime = Date()

		let elapsedTime = endTime.timeIntervalSince(startTime)

		print("parsing \(source.count) for \(ITERATION_COUNT) iterations in \(elapsedTime) seconds: \(Double(source.unicodeScalars.count * ITERATION_COUNT) / Double(elapsedTime)) scalar/s")
	}
}
