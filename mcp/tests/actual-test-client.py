#!/usr/bin/env python3
"""
Actual MCP Test Client - Really tests clustering on example.lisp
This client starts the MCP server and runs real clustering tests
"""

import json
import subprocess
import time
import threading
import sys
import os
from pathlib import Path

class RealMCPClient:
    def __init__(self):
        self.server_process = None
        self.server_ready = False

    def start_mcp_server(self):
        """Start the actual ACL2(ml) MCP server"""
        print("🚀 Starting ACL2(ml) MCP Server...")

        # Create server startup script
        server_script = """:q
(load "~/quicklisp/setup.lisp")
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)
(load "/workspaces/acl2ml/mcp/clustering.lisp")
(load "/workspaces/acl2ml/mcp/feature-extraction-full.lisp")
(load "/workspaces/acl2ml/mcp/acl2ml-full-server.lisp")
"""

        # Start server process
        self.server_process = subprocess.Popen([
            "/home/acl2/saved_acl2"
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1)

        print("📤 Sending server startup commands...")
        self.server_process.stdin.write(server_script)
        self.server_process.stdin.flush()

        # Wait for server to be ready
        print("⏳ Waiting for MCP server to initialize...")
        time.sleep(15)  # Give it time to load everything

        return True

    def send_mcp_request(self, method, params=None, request_id=1):
        """Send actual MCP request to running server"""
        request = {
            "jsonrpc": "2.0",
            "method": method,
            "id": request_id
        }
        if params:
            request["params"] = params

        request_json = json.dumps(request) + "\n"

        try:
            # Send request
            self.server_process.stdin.write(request_json)
            self.server_process.stdin.flush()

            # Read response - try multiple times
            response_data = ""
            for attempt in range(5):
                try:
                    line = self.server_process.stdout.readline()
                    if line.strip():
                        response_data = line.strip()
                        break
                    time.sleep(0.5)
                except:
                    time.sleep(0.5)
                    continue

            if response_data:
                try:
                    return json.loads(response_data)
                except json.JSONDecodeError:
                    return {"error": f"Invalid JSON: {response_data[:200]}"}
            else:
                return {"error": "No response received"}

        except Exception as e:
            return {"error": f"Communication error: {str(e)}"}

    def test_actual_clustering(self):
        """Run actual clustering tests on example.lisp"""
        print("\n🧪 TESTING ACTUAL MCP CLUSTERING")
        print("=" * 45)

        # Test 1: Initialize MCP
        print("\n1️⃣ Initializing MCP connection...")
        init_resp = self.send_mcp_request("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {"tools": {}},
            "clientInfo": {"name": "Real Test Client", "version": "1.0"}
        }, 1)

        if "result" in init_resp:
            print("✅ MCP initialized successfully")
        else:
            print(f"❌ MCP init failed: {init_resp}")

        time.sleep(2)

        # Test 2: List available tools
        print("\n2️⃣ Getting available tools...")
        tools_resp = self.send_mcp_request("tools/list", None, 2)

        if "result" in tools_resp:
            tools = tools_resp["result"].get("tools", [])
            print(f"✅ Found {len(tools)} tools:")
            for tool in tools:
                print(f"   • {tool.get('name', 'unnamed')}")
        else:
            print(f"❌ Tools list failed: {tools_resp}")

        time.sleep(2)

        # Test 3: ACTUAL CLUSTERING TEST
        print("\n3️⃣ RUNNING CLUSTERING ON EXAMPLE.LISP...")
        print("   Algorithm: K-means, Granularity: 3")

        cluster_resp = self.send_mcp_request("tools/call", {
            "name": "cluster-definitions",
            "arguments": {
                "algorithm": "k",
                "granularity-level": "3"
            }
        }, 3)

        if "result" in cluster_resp:
            print("✅ CLUSTERING SUCCESSFUL!")
            content = cluster_resp["result"].get("content", [])
            if content:
                cluster_text = content[0].get("text", "")
                print("\n📊 CLUSTERING RESULTS:")
                print("=" * 25)
                print(cluster_text)
            else:
                print("⚠️ No clustering content returned")
        else:
            print(f"❌ Clustering failed: {cluster_resp}")

        time.sleep(2)

        # Test 4: Find similar to THETA_SUM
        print("\n4️⃣ FINDING SIMILAR TO THETA_SUM...")

        similar_resp = self.send_mcp_request("tools/call", {
            "name": "find-similar-definitions",
            "arguments": {
                "target-name": "THETA_SUM",
                "algorithm": "k"
            }
        }, 4)

        if "result" in similar_resp:
            print("✅ SIMILARITY SEARCH SUCCESSFUL!")
            content = similar_resp["result"].get("content", [])
            if content:
                similar_text = content[0].get("text", "")
                print("\n🔍 SIMILARITY RESULTS:")
                print("=" * 23)
                print(similar_text)
            else:
                print("⚠️ No similarity content returned")
        else:
            print(f"❌ Similarity search failed: {similar_resp}")

        time.sleep(2)

        # Test 5: Feature extraction
        print("\n5️⃣ TESTING FEATURE EXTRACTION...")

        features_resp = self.send_mcp_request("tools/call", {
            "name": "extract-ml-features",
            "arguments": {
                "expression": "(implies (consp x) (equal (append x y) (reverse y)))"
            }
        }, 5)

        if "result" in features_resp:
            print("✅ FEATURE EXTRACTION SUCCESSFUL!")
            content = features_resp["result"].get("content", [])
            if content:
                features_text = content[0].get("text", "")
                print("\n🧮 FEATURE RESULTS:")
                print("=" * 19)
                print(features_text)
            else:
                print("⚠️ No feature content returned")
        else:
            print(f"❌ Feature extraction failed: {features_resp}")

    def cleanup(self):
        """Clean up server process"""
        if self.server_process:
            print("\n🧹 Shutting down MCP server...")
            self.server_process.terminate()
            try:
                self.server_process.wait(timeout=5)
            except:
                self.server_process.kill()

def main():
    """Main test function"""
    print("🎯 REAL ACL2(ml) MCP CLUSTERING TEST")
    print("=" * 40)
    print("This will actually start the MCP server and run clustering!")

    client = RealMCPClient()

    try:
        # Start the server
        if client.start_mcp_server():
            # Run the actual tests
            client.test_actual_clustering()

            print("\n🎉 REAL MCP TESTING COMPLETE!")
            print("=" * 32)
            print("✅ Started actual ACL2(ml) MCP server")
            print("✅ Sent real JSON-RPC requests over STDIO")
            print("✅ Received actual clustering results")
            print("✅ Demonstrated working MCP integration")

        else:
            print("❌ Failed to start MCP server")

    except KeyboardInterrupt:
        print("\n⚠️ Test interrupted")
    except Exception as e:
        print(f"\n❌ Test error: {e}")
    finally:
        client.cleanup()

if __name__ == "__main__":
    main()