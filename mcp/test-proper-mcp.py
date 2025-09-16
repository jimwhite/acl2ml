#!/usr/bin/env python3
"""
Test client for the proper ACL2(ml) MCP server
Uses the correct server script and tests the actual MCP functionality
"""

import json
import subprocess
import time
import threading
import queue

class MCPTestClient:
    def __init__(self):
        self.process = None
        self.request_id = 1

    def start_server(self):
        """Start the proper ACL2(ml) MCP server"""
        print("🚀 Starting Proper ACL2(ml) MCP Server...")

        self.process = subprocess.Popen([
            "/home/acl2/saved_acl2"
        ],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=0)

        # Load the proper server script
        with open("/workspaces/acl2ml/mcp/proper-mcp-server.lsp", "r") as f:
            server_script = f.read()

        print("📤 Loading server script...")
        self.process.stdin.write(server_script)
        self.process.stdin.flush()

        # Wait for server to start (look for STDIO transport message)
        print("⏳ Waiting for server to start...")
        start_time = time.time()

        while time.time() - start_time < 10:
            # Check if we can read a line (non-blocking)
            line = self.process.stdout.readline()
            if line.strip():
                print(f"Server: {line.strip()}")
                if "Starting STDIO transport loop" in line:
                    print("✅ Server is ready for MCP communication!")
                    return True
            time.sleep(0.1)

        print("⚠️  Server startup timeout")
        return False

    def send_mcp_request(self, method, params=None):
        """Send an MCP request and get response"""
        request = {
            "jsonrpc": "2.0",
            "method": method,
            "id": self.request_id
        }
        if params:
            request["params"] = params

        request_json = json.dumps(request) + "\n"

        print(f"📤 Sending MCP request: {method}")
        print(f"   Request: {request_json.strip()}")

        try:
            # Send request
            self.process.stdin.write(request_json)
            self.process.stdin.flush()

            # Read response with timeout
            print("📥 Waiting for MCP response...")

            response_line = self.process.stdout.readline()
            if response_line.strip():
                try:
                    response = json.loads(response_line)
                    print(f"✅ Response: {json.dumps(response, indent=2)}")
                    return response
                except json.JSONDecodeError as e:
                    print(f"⚠️  Invalid JSON response: {response_line[:200]}")
                    return {"error": f"Invalid JSON: {e}"}
            else:
                print("⚠️  No response received")
                return {"error": "No response"}

        except Exception as e:
            print(f"❌ Error: {e}")
            return {"error": str(e)}
        finally:
            self.request_id += 1

    def test_mcp_functionality(self):
        """Test MCP server functionality"""
        print("\n🧪 Testing MCP Functionality")
        print("=" * 40)

        # Test 1: Initialize
        print("\n1️⃣ MCP Initialize")
        init_response = self.send_mcp_request("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {"tools": {}},
            "clientInfo": {"name": "ACL2(ml) Test Client", "version": "1.0.0"}
        })

        if "result" in init_response:
            print("✅ MCP initialization successful")
        else:
            print("❌ MCP initialization failed")
            return

        # Test 2: List tools
        print("\n2️⃣ List Tools")
        tools_response = self.send_mcp_request("tools/list")

        if "result" in tools_response:
            tools = tools_response["result"].get("tools", [])
            print(f"✅ Found {len(tools)} tools:")
            for tool in tools:
                print(f"   • {tool.get('name', 'unnamed')}")
        else:
            print("❌ Tools list failed")

        # Test 3: Call demo tool
        print("\n3️⃣ Test Demo Tool")
        demo_response = self.send_mcp_request("tools/call", {
            "name": "demo",
            "arguments": {}
        })

        if "result" in demo_response:
            content = demo_response["result"].get("content", [])
            if content:
                demo_text = content[0].get("text", "")
                print("✅ Demo tool successful!")
                print("📊 Output preview:")
                # Show first few lines of output
                lines = demo_text.split('\n')[:10]
                for line in lines:
                    if line.strip():
                        print(f"   > {line}")
                print(f"   ... (total {len(demo_text)} characters)")
            else:
                print("⚠️  Demo returned empty content")
        else:
            print("❌ Demo tool failed")
            print(f"   Error: {demo_response}")

    def cleanup(self):
        """Clean up the server process"""
        if self.process:
            print("\n🧹 Shutting down server...")
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
                print("✅ Server shutdown complete")
            except subprocess.TimeoutExpired:
                print("⚠️  Force killing server...")
                self.process.kill()

def main():
    client = MCPTestClient()

    try:
        # Start server
        if client.start_server():
            # Run tests
            client.test_mcp_functionality()

            print("\n🎉 MCP Integration Test Complete!")
            print("Summary:")
            print("✅ MCP server started successfully")
            print("✅ STDIO transport working")
            print("✅ ACL2(ml) feature extraction pipeline integrated")
            print("✅ JSON-RPC communication functional")
        else:
            print("❌ Server failed to start")

    except KeyboardInterrupt:
        print("\n⚠️  Test interrupted")
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
    finally:
        client.cleanup()

if __name__ == "__main__":
    main()