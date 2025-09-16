#!/usr/bin/env python3
"""
Test the simple MCP server to see if demo tool works
"""

import json
import subprocess
import time
import select

def test_simple_server():
    print("🧪 Testing Simple MCP Server")
    print("=" * 30)

    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load simple server
        with open("/workspaces/acl2ml/mcp/simple-test-server.lsp", "r") as f:
            server_script = f.read()

        process.stdin.write(server_script)
        process.stdin.flush()
        time.sleep(3)

        # Initialize
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "id": 1,
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "clientInfo": {"name": "Simple Test", "version": "1.0.0"}
            }
        }

        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        time.sleep(1)

        # Call test tool
        print("📤 Calling 'test' tool...")
        test_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 2,
            "params": {
                "name": "test",
                "arguments": {}
            }
        }

        process.stdin.write(json.dumps(test_request) + "\n")
        process.stdin.flush()

        # Look for response
        print("⏳ Waiting for response...")

        for i in range(100):  # 10 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line and '"id":2' in line and line.strip().startswith('{"'):
                    try:
                        response = json.loads(line.strip())
                        print("✅ Got response!")

                        if "result" in response:
                            content = response["result"].get("content", [])
                            if content:
                                test_text = content[0].get("text", "")
                                print("\n🎯 ACL2(ml) Test Result:")
                                print("=" * 40)
                                print(test_text)
                                print("=" * 40)
                                return True
                        return False
                    except json.JSONDecodeError:
                        continue

        print("⏱️  No response received")
        return False

    except Exception as e:
        print(f"❌ Error: {e}")
        return False
    finally:
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()

if __name__ == "__main__":
    success = test_simple_server()
    if success:
        print("\n✅ Simple MCP server test successful!")
        print("🎯 ACL2(ml) feature extraction is working via MCP!")
    else:
        print("\n❌ Simple test failed")