//
//  AutomataBuilderTests.swift
//
//
//  Created by Larry Zeng on 12/2/23.
//

import Testing
@testable import LexerMacros

protocol IntoByte {
	func into() -> UInt32
}

extension Character: IntoByte {
	func into() -> UInt32 {
		return self.scalarByte
	}
}

extension String: IntoByte {
	func into() -> UInt32 {
		assert(self.count == 1)
		return self.first!.scalarByte
	}
}

extension UInt32: IntoByte {
	func into() -> UInt32 {
		return self
	}
}

extension Int: IntoByte {
	func into() -> UInt32 {
		return UInt32(self)
	}
}

func branch(_ children: [Character: NodeId], _ miss: NodeId? = nil, inverted: Bool = false) -> Node {
	return .Branch(.init(branches: Dictionary(uniqueKeysWithValues: children.map { ($0.scalarByte.scalarByteRange, $1) }), miss: miss))
}

func branch(_ children: [Node.BranchHit: NodeId], _ miss: NodeId? = nil, inverted: Bool = false) -> Node {
	return .Branch(.init(branches: children, miss: miss))
}

func seq(_ seq: String, _ then: NodeId, _ miss: Node.SeqMiss? = nil) -> Node {
	return .Seq(.init(seq: seq.map { $0.into() }, then: then, miss: miss))
}

func range(_ lhs: any IntoByte, _ rhs: any IntoByte) -> Node.BranchHit {
	return lhs.into()...rhs.into()
}

func leaf(_ end: EndsId) -> Node {
	return .Leaf(.init(endId: end))
}

struct AutomataBuilderArgs: Sendable {
	let regexes: [String]
	let nodes: [Node?]
	let root: NodeId
}

@Suite
struct AutomataBuilderTests {
	@Test(arguments: [
		AutomataBuilderArgs(
			regexes: ["ab"],
			nodes: [seq("ab", 1), leaf(0)],
			root: 0),
		AutomataBuilderArgs(
			regexes: ["ab", "ab(b)+?"],
			nodes: [seq("b", 3, .first(5)), branch(["b": 0]), branch(["a": 1]), seq("b", 3, .first(4)), leaf(1), leaf(0)],
			root: 2),
		AutomataBuilderArgs(
			regexes: ["ab", "[a-b]+?"],
			nodes: [branch([range("a", "b"): 3], 5), seq("b", 0, .anytime(3)), branch(["a": 1, "b": 3]), branch([range("a", "b"): 3], 4), leaf(1), leaf(0)],
			root: 2),
		AutomataBuilderArgs(
			regexes: ["ab", "[^a]"],
			nodes: [seq("b", 3), branch([0...96: 2, 98...HIR.ScalarByte.max: 2, 97...97: 0]), leaf(1), leaf(0)],
			root: 1),
		AutomataBuilderArgs(
			regexes: ["ab", "[^bc]+?"],
			nodes: [branch([0...97: 2, 98...98: 4, 100...HIR.ScalarByte.max: 2], 3), branch([97...97: 0, 100...HIR.ScalarByte.max: 2, 0...96: 2]), branch([0...97: 2, 100...HIR.ScalarByte.max: 2], 3), leaf(1), leaf(0)],
			root: 1)
	])
	func buildGraph(args: AutomataBuilderArgs) throws {
		var graph = Graph()
		for regexContent in args.regexes {
			let hir = try HIR(regex: regexContent)
			try graph.push(input: .init(token: "LEAF", tokenType: .standalone, hir: hir))
		}

		_ = try graph.makeRoot()
		_ = try graph.shake()

		#expect(graph.nodes == args.nodes)
		#expect(graph.rootId == args.root)
	}
}
