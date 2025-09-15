#!/usr/bin/env python3
"""
Test MCP client for ACL2(ml) server
This demonstrates calling the ACL2(ml) MCP tools
"""

import json
import sys

def send_mcp_request(method, params=None, request_id=1):
    """Send an MCP JSON-RPC request"""
    request = {
        "jsonrpc": "2.0",
        "method": method,
        "id": request_id
    }
    if params:
        request["params"] = params

    print(f"📤 Sending MCP request:")
    print(json.dumps(request, indent=2))
    print()
    return request

def demo_acl2ml_tools():
    """Demo the ACL2(ml) MCP tools"""
    print("🧠 ACL2(ml) MCP Client Demo")
    print("=" * 40)
    print()

    # Test 1: Initialize MCP connection
    print("1. Testing MCP initialization...")
    init_request = send_mcp_request("initialize", {
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {"name": "ACL2(ml) Demo Client", "version": "1.0.0"}
    })

    # Test 2: List available tools
    print("2. Listing available tools...")
    tools_request = send_mcp_request("tools/list", request_id=2)

    # Test 3: Call analyze-expression tool
    print("3. Testing expression analysis...")
    analyze_request = send_mcp_request("tools/call", {
        "name": "analyze-expression",
        "arguments": {
            "expression": "(implies (consp x) (equal x x))"
        }
    }, request_id=3)

    # Test 4: Call analyze-theorem tool
    print("4. Testing theorem analysis...")
    theorem_request = send_mcp_request("tools/call", {
        "name": "analyze-theorem",
        "arguments": {
            "theorem-expression": "(defthm append-associative (implies (and (true-listp x) (true-listp y)) (equal (append (append x y) z) (append x (append y z)))))"
        }
    }, request_id=4)

    # Test 5: Demo tool
    print("5. Testing ACL2(ml) demo...")
    demo_request = send_mcp_request("tools/call", {
        "name": "demo-acl2ml",
        "arguments": {}
    }, request_id=5)

    print()
    print("🎯 Expected Results:")
    print("=" * 20)
    print("• Initialize: Server info and capabilities")
    print("• Tools list: analyze-expression, analyze-theorem, demo-acl2ml")
    print("• Expression analysis: ML features, depth, complexity")
    print("• Theorem analysis: Logical structure, proof complexity")
    print("• Demo: System status and capabilities overview")
    print()
    print("💡 These requests would be sent to the ACL2(ml) MCP server")
    print("   The server extracts ML features from ACL2 expressions")
    print("   and provides AI-powered theorem proving assistance!")

if __name__ == "__main__":
    demo_acl2ml_tools()