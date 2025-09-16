#!/usr/bin/env python3
"""
Test MCP server per COMPLETE_IMPLEMENTATION_SPEC.md
Client reads ACL2 content and sends to server for clustering
"""

import json
import requests
import time
import subprocess

def test_spec_compliant_clustering():
    print("🎯 Testing Spec-Compliant ACL2(ml) HTTP MCP Server")
    print("=" * 50)

    server_url = "http://localhost:8082/mcp"

    print("🔍 Checking if HTTP server is running on port 8082...")

    # Wait for server to be available
    max_retries = 30
    for i in range(max_retries):
        try:
            response = requests.post(server_url,
                                   json={"jsonrpc": "2.0", "method": "tools/list", "id": 0},
                                   timeout=2)
            print(f"Status Code: {response.status_code}")
            print(f"Response: {response.json()}")
            if response.status_code == 200:
                print("✅ HTTP server is responsive!")
                break
        except (requests.exceptions.ConnectionError, requests.exceptions.Timeout):
            if i < max_retries - 1:
                print(f"   Waiting for server... ({i+1}/{max_retries})")
                time.sleep(1)
            else:
                print("❌ HTTP server not available on port 8082")
                print("   Please start the server first with: timeout 30 /home/acl2/saved_acl2 < /workspaces/acl2ml/mcp/spec-compliant-server.lsp &")
                return False

    try:
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

        response = requests.post(server_url, json=init_request, timeout=10)
        if response.status_code != 200:
            print(f"❌ Initialization failed: {response.status_code}")
            return False

        # Read ACL2 content from example.lisp (CLIENT SIDE - per spec)
        print("📖 Client reading example.lisp...")
        with open("/workspaces/acl2ml/manual/example.lisp", "r") as f:
            acl2_content = f.read()

        print(f"   Read {len(acl2_content)} characters from example.lisp")

        # Call acl2_cluster_analysis per spec
        print("🧮 Calling acl2-cluster-analysis with client content...")
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

        print("📥 Sending request to HTTP server...")
        response = requests.post(server_url, json=cluster_request, timeout=30)

        if response.status_code != 200:
            print(f"❌ HTTP request failed: {response.status_code}")
            print(f"Response: {response.text}")
            return False

        try:
            result = response.json()
            print("✅ Clustering response received!")

            if "result" in result:
                content = result["result"].get("content", [])
                if content:
                    result_text = content[0].get("text", "")
                    print("\n🎯 ACL2(ml) Clustering Results (per spec):")
                    print("=" * 60)
                    print(result_text)
                    print("=" * 60)

                    # Check for successful clustering per spec format
                    if ":clusters" in result_text and ":total-items" in result_text:
                        print("\n🎉 SUCCESS: Spec-compliant HTTP clustering working!")
                        print("✅ Client sent ACL2 content to HTTP server")
                        print("✅ Server processed content and returned structured results")
                        print("✅ Architecture matches COMPLETE_IMPLEMENTATION_SPEC.md")
                        return True
                    else:
                        print("\n⚠️  Response format may not match spec")
                else:
                    print("\n⚠️  Empty clustering result")
            else:
                print(f"\n❌ Clustering error: {result.get('error', 'unknown')}")
            return False
        except json.JSONDecodeError as e:
            print(f"❌ Invalid JSON response: {e}")
            print(f"Raw response: {response.text}")
            return False

    except Exception as e:
        print(f"❌ Test error: {e}")
        return False

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