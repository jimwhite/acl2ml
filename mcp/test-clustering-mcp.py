#!/usr/bin/env python3
"""
Test ACL2(ml) clustering via MCP - the minimal test
"""

import json
import subprocess
import time
import select

def test_clustering_mcp():
    print("🎯 Testing ACL2(ml) Clustering via MCP")
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
        # Load clustering server
        with open("/workspaces/acl2ml/mcp/clustering-mcp-server.lsp", "r") as f:
            server_script = f.read()

        print("📤 Loading clustering server...")
        process.stdin.write(server_script)
        process.stdin.flush()
        time.sleep(5)

        # Initialize MCP
        print("🔧 Initializing MCP...")
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "id": 1,
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "clientInfo": {"name": "Clustering Test", "version": "1.0.0"}
            }
        }

        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        time.sleep(2)

        # List tools to verify clustering tool is available
        print("📋 Listing available tools...")
        tools_request = {
            "jsonrpc": "2.0",
            "method": "tools/list",
            "id": 2
        }

        process.stdin.write(json.dumps(tools_request) + "\n")
        process.stdin.flush()
        time.sleep(1)

        # Call clustering tool - the main test
        print("🧮 Calling clustering tool...")
        cluster_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 3,
            "params": {
                "name": "cluster",
                "arguments": {}
            }
        }

        process.stdin.write(json.dumps(cluster_request) + "\n")
        process.stdin.flush()

        # Read responses
        print("📥 Reading responses...")
        responses = []

        for i in range(300):  # 30 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line and line.strip().startswith('{"'):
                    try:
                        response = json.loads(line.strip())
                        responses.append(response)

                        if response.get("id") == 2:  # tools/list
                            tools = response["result"].get("tools", [])
                            print(f"   ✅ Found {len(tools)} tools:")
                            for tool in tools:
                                print(f"      • {tool.get('name')}: {tool.get('description', 'no description')}")

                        elif response.get("id") == 3:  # clustering call
                            print("   ✅ Clustering response received!")
                            if "result" in response:
                                content = response["result"].get("content", [])
                                if content:
                                    cluster_text = content[0].get("text", "")
                                    print("\n🎯 ACL2(ml) Clustering Results:")
                                    print("=" * 50)
                                    print(cluster_text)
                                    print("=" * 50)

                                    # Check for successful clustering
                                    if "Clusters found:" in cluster_text and "✅ Clustering complete!" in cluster_text:
                                        print("\n🎉 SUCCESS: ACL2(ml) clustering working via MCP!")
                                        return True
                                    else:
                                        print("\n⚠️  Clustering ran but may not have completed successfully")
                                        return False
                                else:
                                    print("\n⚠️  Empty clustering result")
                                    return False
                            else:
                                print(f"\n❌ Clustering error: {response.get('error', 'unknown error')}")
                                return False
                    except json.JSONDecodeError:
                        continue

            # Progress indicator
            if i % 50 == 0 and i > 0:
                print(f"   Still waiting... ({i//10}s)")

        print("⏱️  Timeout waiting for clustering response")
        return False

    except Exception as e:
        print(f"❌ Test error: {e}")
        return False
    finally:
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()

if __name__ == "__main__":
    success = test_clustering_mcp()
    if success:
        print("\n🎉 ACL2(ml) MCP clustering test PASSED!")
        print("✅ Complete integration working:")
        print("   • MCP server receiving requests")
        print("   • ACL2(ml) pipeline processing definitions")
        print("   • Feature extraction creating vectors")
        print("   • Clustering algorithm organizing results")
        print("   • Results returned via MCP protocol")
    else:
        print("\n❌ ACL2(ml) MCP clustering test FAILED")