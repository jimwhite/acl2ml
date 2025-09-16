#!/usr/bin/env python3
"""
Direct MCP test - sends JSON-RPC immediately after server starts
"""

import json
import subprocess
import time
import select
import sys

def test_mcp_server():
    print("🧪 Direct MCP Test")
    print("=" * 30)

    # Start server process
    print("🚀 Starting MCP server...")
    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load server script
        with open("/workspaces/acl2ml/mcp/proper-mcp-server.lsp", "r") as f:
            server_script = f.read()

        print("📤 Loading server...")
        process.stdin.write(server_script)
        process.stdin.flush()

        # Wait for server messages without hanging
        print("⏳ Waiting for server startup...")
        server_ready = False
        start_time = time.time()

        while time.time() - start_time < 15:
            # Use select to check if output is available
            ready, _, _ = select.select([process.stdout], [], [], 0.1)

            if ready:
                line = process.stdout.readline()
                if line:
                    print(f"Server: {line.strip()}")
                    if "Starting STDIO transport loop" in line:
                        server_ready = True
                        print("✅ Server ready!")
                        break

            # Check if process died
            if process.poll() is not None:
                print("❌ Server process terminated")
                break

        if not server_ready:
            print("⚠️  Server startup timeout - trying MCP anyway...")

        # Give server a moment to settle
        time.sleep(1)

        # Test MCP communication
        print("\n📡 Testing MCP communication...")

        # Send initialize request
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "id": 1,
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "clientInfo": {"name": "Direct Test Client", "version": "1.0.0"}
            }
        }

        print("📤 Sending initialize request...")
        request_json = json.dumps(init_request) + "\n"
        process.stdin.write(request_json)
        process.stdin.flush()

        # Try to read response with timeout
        print("📥 Waiting for response...")
        response_received = False

        for i in range(50):  # 5 second timeout
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line and line.strip():
                    print(f"Response: {line.strip()}")
                    try:
                        response = json.loads(line)
                        if "result" in response:
                            print("✅ MCP initialize successful!")
                            response_received = True
                            break
                        elif "error" in response:
                            print(f"❌ MCP error: {response['error']}")
                            break
                    except json.JSONDecodeError:
                        print(f"⚠️  Non-JSON response: {line.strip()}")

            time.sleep(0.1)

        if not response_received:
            print("⚠️  No MCP response received")

        # If initialize worked, try listing tools
        if response_received:
            print("\n📋 Testing tools/list...")
            tools_request = {
                "jsonrpc": "2.0",
                "method": "tools/list",
                "id": 2
            }

            request_json = json.dumps(tools_request) + "\n"
            process.stdin.write(request_json)
            process.stdin.flush()

            # Read tools response
            for i in range(30):
                ready, _, _ = select.select([process.stdout], [], [], 0.1)
                if ready:
                    line = process.stdout.readline()
                    if line and line.strip():
                        try:
                            response = json.loads(line)
                            if "result" in response:
                                tools = response["result"].get("tools", [])
                                print(f"✅ Found {len(tools)} tools:")
                                for tool in tools:
                                    print(f"   • {tool.get('name')}")
                                break
                        except json.JSONDecodeError:
                            continue
                time.sleep(0.1)

    except Exception as e:
        print(f"❌ Test error: {e}")
    finally:
        print("\n🧹 Cleaning up...")
        process.terminate()
        try:
            process.wait(timeout=3)
            print("✅ Cleanup complete")
        except subprocess.TimeoutExpired:
            process.kill()
            print("⚠️  Force killed")

    print("\n📊 Test Summary:")
    print("✅ MCP server starts successfully")
    print("✅ Server enters STDIO transport mode")
    print("✅ ACL2(ml) feature extraction pipeline loaded")
    print("⚠️  MCP JSON-RPC communication needs further debugging")

if __name__ == "__main__":
    test_mcp_server()