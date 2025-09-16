#!/usr/bin/env python3
"""
Test simple spec-compliant server with working plumbing
"""

import json
import subprocess
import time
import select

def test_simple_spec():
    print("🧪 Testing Simple Spec-Compliant Server")
    print("=" * 40)

    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load simple spec server
        with open("/workspaces/acl2ml/mcp/simple-spec-server.lsp", "r") as f:
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
                "clientInfo": {"name": "Simple Spec Test", "version": "1.0.0"}
            }
        }

        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        time.sleep(1)

        # Read ACL2 content from example.lisp (CLIENT SIDE)
        with open("/workspaces/acl2ml/manual/example.lisp", "r") as f:
            acl2_content = f.read()

        print(f"📖 Client read {len(acl2_content)} characters from example.lisp")

        # Call clustering tool with content
        print("🧮 Calling cluster tool with content...")
        cluster_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 2,
            "params": {
                "name": "cluster",
                "arguments": {
                    "content": acl2_content
                }
            }
        }

        process.stdin.write(json.dumps(cluster_request) + "\n")
        process.stdin.flush()

        # Look for response
        print("⏳ Waiting for response...")

        for i in range(200):  # 20 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line and '"id":2' in line and line.strip().startswith('{"'):
                    try:
                        response = json.loads(line.strip())
                        print("✅ Got clustering response!")

                        if "result" in response:
                            content = response["result"].get("content", [])
                            if content:
                                result_text = content[0].get("text", "")
                                print("\n🎯 Clustering Results:")
                                print("=" * 40)
                                print(result_text)
                                print("=" * 40)

                                if "Clustering complete!" in result_text:
                                    print("\n🎉 SUCCESS: Simple spec-compliant clustering working!")
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
    success = test_simple_spec()
    if success:
        print("\n✅ Simple spec-compliant MCP server working!")
        print("🎯 Architecture validated: Client sends content → Server clusters → Results returned")
    else:
        print("\n❌ Test failed")