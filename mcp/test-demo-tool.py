#!/usr/bin/env python3
"""
Test specifically the ACL2(ml) demo tool execution
"""

import json
import subprocess
import time
import select

def test_demo_tool():
    print("🎯 Testing ACL2(ml) Demo Tool")
    print("=" * 30)

    # Start server
    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load server
        with open("/workspaces/acl2ml/mcp/clean-stdio-server.lsp", "r") as f:
            server_script = f.read()

        process.stdin.write(server_script)
        process.stdin.flush()
        time.sleep(5)

        # Initialize first
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "id": 1,
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "clientInfo": {"name": "Demo Test", "version": "1.0.0"}
            }
        }

        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        time.sleep(2)

        # Call demo tool
        print("📤 Calling ACL2(ml) demo tool...")
        demo_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 2,
            "params": {
                "name": "demo",
                "arguments": {}
            }
        }

        process.stdin.write(json.dumps(demo_request) + "\n")
        process.stdin.flush()

        # Wait longer for demo tool response
        print("⏳ Waiting for demo tool response...")

        for i in range(300):  # 30 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line:
                    # Look for JSON response to our demo call
                    if line.strip().startswith('{"') and '"id":2' in line:
                        try:
                            response = json.loads(line.strip())
                            print("✅ Demo tool response received!")

                            if "result" in response:
                                content = response["result"].get("content", [])
                                if content:
                                    demo_text = content[0].get("text", "")
                                    print(f"\n🎯 ACL2(ml) Demo Results:")
                                    print("=" * 40)
                                    print(demo_text)
                                    print("=" * 40)

                                    # Check if feature extraction worked
                                    if "feature vectors" in demo_text.lower():
                                        print("\n✅ Feature extraction pipeline executed successfully!")
                                        return True
                                    else:
                                        print("\n⚠️  Demo ran but no feature vectors mentioned")
                                        return False
                                else:
                                    print("⚠️  Demo response has no content")
                                    return False
                            elif "error" in response:
                                print(f"❌ Demo tool error: {response['error']}")
                                return False
                        except json.JSONDecodeError:
                            continue

            # Show progress
            if i % 50 == 0 and i > 0:
                print(f"   Still waiting... ({i//10}s)")

        print("⏱️  Timeout waiting for demo response")
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
    success = test_demo_tool()
    if success:
        print("\n🎉 ACL2(ml) demo tool is working!")
        print("✅ Feature extraction pipeline integrated with MCP")
    else:
        print("\n❌ Demo tool test failed")