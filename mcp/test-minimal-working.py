#!/usr/bin/env python3
"""
Test minimal working server with content parameter
"""

import json
import subprocess
import time
import select

def test_minimal_working():
    print("🧪 Testing Minimal Working Server")
    print("=" * 35)

    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load minimal working server
        with open("/workspaces/acl2ml/mcp/minimal-working-server.lsp", "r") as f:
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
                "clientInfo": {"name": "Minimal Test", "version": "1.0.0"}
            }
        }

        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        time.sleep(1)

        # Test with small content
        test_content = "(defun simple-test (x) (+ x 1))"
        print(f"📤 Sending test content: {test_content}")

        # Call test tool with content
        test_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 2,
            "params": {
                "name": "test",
                "arguments": {
                    "content": test_content
                }
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
                                result_text = content[0].get("text", "")
                                print(f"\n🎯 Result: {result_text}")

                                if "Content received:" in result_text:
                                    print("\n🎉 SUCCESS: Minimal working server with parameter!")
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
    success = test_minimal_working()
    if success:
        print("\n✅ Minimal working server with parameters validated!")
        print("🔄 Ready to add clustering functionality")
    else:
        print("\n❌ Minimal test failed")