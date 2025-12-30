//
//  TokenizerTests.swift
//
//
//  Created by Larry Zeng on 12/7/23.
//

import Testing
import XCTest
import Lexer
import LexerMacros

@lexer()
enum PriorityTest: Equatable {
	@token("fast")
	case Fast

	@token("fast", priority: 10)
	case Faaaast
}

nonisolated(unsafe) let convertInt: FillCallback<CallbackTest, Int> = { lexer in Int(lexer.rawSlice)! }

nonisolated(unsafe) let convertDouble: FillCallback<CallbackTest, Double> = { lexer in Double(lexer.rawSlice)! }

nonisolated(unsafe) let toSubstring: FillCallback<CallbackTest, Substring> = { lexer in lexer.rawSlice }

nonisolated(unsafe) let questionTokenGen: CreateCallback<CallbackTest, CallbackTest> = { lexer in
	if lexer.rawSlice.count % 2 == 0 {
		return .Question(0)
	} else {
		return .Question(lexer.rawSlice.count)
	}
}

nonisolated(unsafe) let excTokenGen: CreateCallback<CallbackTest, TokenResult<CallbackTest>> = { lexer in
	if lexer.rawSlice.count % 2 == 0 {
		return CallbackTest.Exc.into()
	} else {
		return .skipped
	}
}

@lexer(skip: " ")
enum CallbackTest: Equatable {
	@regex(/[0-9]*?\.[0-9]+?/, fillCallback: convertDouble)
	case Double(Double)

	@regex(/[0-9]+?/, fillCallback: convertInt)
	case Number(Int)

	@token("what", fillCallback: toSubstring)
	case What(Substring)

	@regex(/\/\/.*?/, fillCallback: toSubstring)
	case Comment(Substring)

	@token(".")
	case Dot

	@regex(/\?*?/, createCallback: questionTokenGen)
	case Question(Int)

	@regex(/!*?/, priority: 2, createCallback: excTokenGen)
	case Exc
}

final class TestTokenizer: XCTestCase {
	func testPriority() throws {
		XCTAssertEqual(PriorityTest.lexer(source: "fast").toUnwrappedArray(), [PriorityTest.Faaaast])
	}

	func testCallback() throws {
		XCTAssertEqual(
			CallbackTest.lexer(source: "100 1.5 .6 what . ? ??? ???? !! ! // this is a comment").toUnwrappedArray(),
			[.Number(100), .Double(1.5), .Double(0.6), .What("what"), .Dot, .Question(1), .Question(3), .Question(0), .Exc, .Comment("// this is a comment")]
		)
	}
}
