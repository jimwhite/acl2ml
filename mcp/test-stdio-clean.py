#!/usr/bin/env python3
"""
Clean STDIO MCP test - sends JSON-RPC immediately without waiting for complex output
"""

import json
import subprocess
import time
import select

def test_stdio_mcp():
    print("🧪 Clean STDIO MCP Test")
    print("=" * 30)

    # Start server
    print("🚀 Starting clean STDIO MCP server...")
    process = subprocess.Popen([
        "/home/acl2/saved_acl2"
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0)

    try:
        # Load clean server script
        with open("/workspaces/acl2ml/mcp/clean-stdio-server.lsp", "r") as f:
            server_script = f.read()

        print("📤 Loading clean server...")
        process.stdin.write(server_script)
        process.stdin.flush()

        # Wait briefly for server startup
        print("⏳ Waiting for server...")
        time.sleep(5)

        # Send MCP initialize request
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "id": 1,
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "clientInfo": {"name": "Clean Test", "version": "1.0.0"}
            }
        }

        print("📤 Sending initialize request...")
        print(f"Request: {json.dumps(init_request)}")

        request_json = json.dumps(init_request) + "\n"
        process.stdin.write(request_json)
        process.stdin.flush()

        # Read all available output
        print("📥 Reading all output...")
        output_lines = []

        for i in range(100):  # Read for 10 seconds
            ready, _, _ = select.select([process.stdout], [], [], 0.1)
            if ready:
                line = process.stdout.readline()
                if line:
                    output_lines.append(line.strip())
                    # print(f"Output: {line.strip()}")

                    # Look for JSON response
                    if line.strip().startswith('{"'):
                        try:
                            response = json.loads(line.strip())
                            print(f"✅ JSON Response: {json.dumps(response, indent=2)}")
                            return True
                        except json.JSONDecodeError:
                            continue
            else:
                time.sleep(0.1)

        print(f"📊 Total lines read: {len(output_lines)}")
        print("⚠️  No JSON response found")

        return False

    except Exception as e:
        print(f"❌ Error: {e}")
        return False
    finally:
        print("🧹 Cleaning up...")
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()

if __name__ == "__main__":
    success = test_stdio_mcp()
    if success:
        print("✅ STDIO MCP test successful!")
    else:
        print("❌ STDIO MCP test failed")