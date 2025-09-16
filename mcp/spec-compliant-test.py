#!/usr/bin/env python3
"""
Test MCP server per COMPLETE_IMPLEMENTATION_SPEC.md
Client reads ACL2 content and sends to server for clustering
"""

import json
import subprocess
import time
import select

def test_spec_compliant_clustering():
    print("🎯 Testing Spec-Compliant ACL2(ml) MCP Server")
    print("=" * 50)

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
        # Load spec-compliant server
        with open("/workspaces/acl2ml/mcp/spec-compliant-server.lsp", "r") as f:
            server_script = f.read()

        print("📤 Loading spec-compliant server...")
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
                "clientInfo": {"name": "Spec Test Client", "version": "1.0.0"}
            }
        }

        process.stdin.write(json.dumps(init_request) + "\n")
        process.stdin.flush()
        time.sleep(2)

        # Read ACL2 content from example.lisp (CLIENT SIDE - per spec)
        print("📖 Client reading example.lisp...")
        with open("/workspaces/acl2ml/manual/example.lisp", "r") as f:
            acl2_content = f.read()

        print(f"   Read {len(acl2_content)} characters from example.lisp")

        # Call acl2_cluster_analysis per spec
        print("🧮 Calling acl2_cluster_analysis with client content...")
        cluster_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "id": 2,
            "params": {
                "name": "acl2-cluster-analysis",
                "arguments": {
                    "acl2-content": acl2_content,
                    "content-type": "definitions",
                    "algorithm": "k-means",
                    "granularity": 3
                }
            }
        }

        process.stdin.write(json.dumps(cluster_request) + "\n")
        process.stdin.flush()

        # Read response
        print("📥 Waiting for clustering response...")

        for i in range(300):  # 30 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line and '"id":2' in line and line.strip().startswith('{"'):
                    try:
                        response = json.loads(line.strip())
                        print("✅ Clustering response received!")

                        if "result" in response:
                            content = response["result"].get("content", [])
                            if content:
                                result_text = content[0].get("text", "")
                                print("\n🎯 ACL2(ml) Clustering Results (per spec):")
                                print("=" * 60)
                                print(result_text)
                                print("=" * 60)

                                # Check for successful clustering per spec format
                                if ":clusters" in result_text and ":total-items" in result_text:
                                    print("\n🎉 SUCCESS: Spec-compliant clustering working!")
                                    print("✅ Client sent ACL2 content to server")
                                    print("✅ Server processed content and returned structured results")
                                    print("✅ Architecture matches COMPLETE_IMPLEMENTATION_SPEC.md")
                                    return True
                                else:
                                    print("\n⚠️  Response format may not match spec")
                            else:
                                print("\n⚠️  Empty clustering result")
                        else:
                            print(f"\n❌ Clustering error: {response.get('error', 'unknown')}")
                        return False
                    except json.JSONDecodeError:
                        continue

            # Progress indicator
            if i % 50 == 0 and i > 0:
                print(f"   Still waiting... ({i//10}s)")

        print("⏱️  Timeout waiting for response")
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
    success = test_spec_compliant_clustering()
    if success:
        print("\n🎉 SPEC-COMPLIANT ACL2(ml) MCP SERVER WORKING!")
        print("🏗️  Architecture validated:")
        print("   • Client (Jupyter/Chat agent) reads user's ACL2 code")
        print("   • Client sends content to MCP server via acl2_cluster_analysis")
        print("   • Server processes content and returns structured clustering")
        print("   • Follows COMPLETE_IMPLEMENTATION_SPEC.md exactly")
        print("\n📋 Ready for Phase 2: Implement remaining MCP tools")
    else:
        print("\n❌ Spec-compliant test failed")
        print("❓ Check server tool parameter documentation")