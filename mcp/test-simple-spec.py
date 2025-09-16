#!/usr/bin/env python3
"""
Test simple spec-compliant server with working plumbing
"""

import json
import requests
import time
import subprocess

def test_simple_spec():
    print("🧪 Testing Simple Spec-Compliant HTTP Server")
    print("=" * 40)

    server_url = "http://localhost:8081"

    print("🔍 Checking if HTTP server is running on port 8081...")

    # Wait for server to be available
    max_retries = 30
    for i in range(max_retries):
        try:
            response = requests.post(f"{server_url}/",
                                   json={"jsonrpc": "2.0", "method": "tools/list", "id": 0},
                                   timeout=2)
            if response.status_code == 200:
                print("✅ HTTP server is responsive!")
                break
        except (requests.exceptions.ConnectionError, requests.exceptions.Timeout):
            if i < max_retries - 1:
                print(f"   Waiting for server... ({i+1}/{max_retries})")
                time.sleep(1)
            else:
                print("❌ HTTP server not available on port 8081")
                print("   Please start the server first with: timeout 30 /home/acl2/saved_acl2 < /workspaces/acl2ml/mcp/simple-spec-server-http.lsp &")
                return False

    try:
        # Initialize
        print("🔧 Initializing MCP...")
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

        response = requests.post(f"{server_url}/", json=init_request, timeout=10)
        if response.status_code != 200:
            print(f"❌ Initialization failed: {response.status_code}")
            return False

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

        print("📥 Sending request to HTTP server...")
        response = requests.post(f"{server_url}/", json=cluster_request, timeout=30)

        if response.status_code != 200:
            print(f"❌ HTTP request failed: {response.status_code}")
            print(f"Response: {response.text}")
            return False

        try:
            result = response.json()
            print("✅ Got clustering response!")

            if "result" in result:
                content = result["result"].get("content", [])
                if content:
                    result_text = content[0].get("text", "")
                    print("\n🎯 Clustering Results:")
                    print("=" * 40)
                    print(result_text)
                    print("=" * 40)

                    if "Clustering complete!" in result_text:
                        print("\n🎉 SUCCESS: Simple spec-compliant HTTP clustering working!")
                        return True
            return False
        except json.JSONDecodeError as e:
            print(f"❌ Invalid JSON response: {e}")
            print(f"Raw response: {response.text}")
            return False

    except Exception as e:
        print(f"❌ Error: {e}")
        return False

if __name__ == "__main__":
    success = test_simple_spec()
    if success:
        print("\n✅ Simple spec-compliant MCP server working!")
        print("🎯 Architecture validated: Client sends content → Server clusters → Results returned")
    else:
        print("\n❌ Test failed")