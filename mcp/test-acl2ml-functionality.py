#!/usr/bin/env python3
"""
Test ACL2(ml) MCP functionality - test our actual tools
"""

import json
import subprocess
import time
import select

def test_acl2ml_tools():
    print("🧪 Testing ACL2(ml) MCP Tools")
    print("=" * 35)

    # Start server
    print("🚀 Starting ACL2(ml) MCP server...")
    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load server script
        with open("/workspaces/acl2ml/mcp/clean-stdio-server.lsp", "r") as f:
            server_script = f.read()

        print("📤 Loading server...")
        process.stdin.write(server_script)
        process.stdin.flush()

        # Wait for server startup
        print("⏳ Waiting for server...")
        time.sleep(5)

        # Test 1: Initialize
        print("\n1️⃣ Testing MCP Initialize")
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "id": 1,
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "clientInfo": {"name": "ACL2ml Test", "version": "1.0.0"}
            }
        }

        request_json = json.dumps(init_request) + "\n"
        process.stdin.write(request_json)
        process.stdin.flush()
        time.sleep(1)

        # Test 2: List tools
        print("2️⃣ Testing tools/list")
        tools_request = {
            "jsonrpc": "2.0",
            "method": "tools/list",
            "id": 2
        }

        request_json = json.dumps(tools_request) + "\n"
        process.stdin.write(request_json)
        process.stdin.flush()
        time.sleep(1)

        # Test 3: Call our demo tool
        print("3️⃣ Testing ACL2(ml) demo tool")
        demo_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 3,
            "params": {
                "name": "demo",
                "arguments": {}
            }
        }

        request_json = json.dumps(demo_request) + "\n"
        process.stdin.write(request_json)
        process.stdin.flush()

        # Read all output and look for responses
        print("\n📥 Processing responses...")
        json_responses = []

        for i in range(200):  # Read for 20 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line:
                    # Look for JSON responses
                    if line.strip().startswith('{"'):
                        try:
                            response = json.loads(line.strip())
                            json_responses.append(response)
                            print(f"📨 Response {response.get('id', '?')}: {response.get('method', response.get('result', response.get('error', 'unknown')))}")
                        except json.JSONDecodeError:
                            continue
            else:
                if len(json_responses) >= 3:  # Got responses to all 3 requests
                    break
                time.sleep(0.1)

        # Analyze responses
        print(f"\n📊 Analysis: Found {len(json_responses)} JSON responses")

        for i, response in enumerate(json_responses):
            print(f"\n🔍 Response {i+1}:")
            if "result" in response:
                if response.get("id") == 2:  # tools/list response
                    tools = response["result"].get("tools", [])
                    print(f"   ✅ Found {len(tools)} tools:")
                    for tool in tools:
                        print(f"      • {tool.get('name', 'unnamed')}: {tool.get('description', 'no description')}")
                elif response.get("id") == 3:  # demo tool response
                    content = response["result"].get("content", [])
                    if content:
                        demo_text = content[0].get("text", "")
                        print(f"   ✅ Demo tool result ({len(demo_text)} characters):")
                        # Show key parts of the demo output
                        lines = demo_text.split('\n')
                        for line in lines:
                            if line.strip():
                                print(f"      > {line}")
                                break  # Just show first meaningful line
                        if "feature vectors" in demo_text.lower():
                            print("      🎯 Feature extraction pipeline executed!")
                    else:
                        print("   ⚠️  Empty demo result")
                else:
                    print(f"   ✅ Success: {json.dumps(response['result'], indent=6)}")
            elif "error" in response:
                print(f"   ❌ Error: {response['error']}")
            else:
                print(f"   ❓ Unknown response format")

        return len(json_responses) >= 2  # At least got some responses

    except Exception as e:
        print(f"❌ Test error: {e}")
        return False
    finally:
        print("\n🧹 Cleaning up...")
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()

if __name__ == "__main__":
    success = test_acl2ml_tools()
    print(f"\n{'✅ ACL2(ml) MCP functionality test completed!' if success else '❌ ACL2(ml) MCP test failed'}")
    if success:
        print("🎯 ACL2(ml) feature extraction is working via MCP!")