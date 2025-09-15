#!/usr/bin/env python3
"""
Test the fixed MCP server
"""

import json
import subprocess
import time
import sys

def test_fixed_mcp():
    print("Testing Fixed MCP Server...")

    # Start server
    proc = subprocess.Popen(
        ["/home/acl2/saved_acl2", "-"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    # Send server script
    with open("/workspaces/acl2ml/mcp/fixed-mcp-server.lisp", "r") as f:
        server_script = f.read()

    proc.stdin.write(server_script)
    proc.stdin.flush()

    # Wait for server to start
    time.sleep(10)

    # Test MCP requests
    requests = [
        {"jsonrpc": "2.0", "method": "initialize", "id": 1,
         "params": {"protocolVersion": "2024-11-05", "capabilities": {}}},
        {"jsonrpc": "2.0", "method": "tools/list", "id": 2},
        {"jsonrpc": "2.0", "method": "tools/call", "id": 3,
         "params": {"name": "cluster-example", "arguments": {}}},
        {"jsonrpc": "2.0", "method": "tools/call", "id": 4,
         "params": {"name": "find-similar", "arguments": {"target-name": "THETA_SUM"}}}
    ]

    for i, req in enumerate(requests):
        print(f"\nSending request {i+1}: {req['method']}")

        try:
            proc.stdin.write(json.dumps(req) + "\n")
            proc.stdin.flush()

            # Try to read response
            response = proc.stdout.readline()
            if response.strip():
                try:
                    resp_obj = json.loads(response)
                    print(f"✅ Response: {resp_obj.get('id', 'unknown')} - {len(str(resp_obj))} chars")
                    if req['method'] == 'tools/call' and 'result' in resp_obj:
                        content = resp_obj['result'].get('content', [])
                        if content:
                            text = content[0].get('text', '')[:200]
                            print(f"   Content preview: {text}...")
                except json.JSONDecodeError:
                    print(f"⚠️  Invalid JSON: {response[:100]}...")
            else:
                print("⚠️  No response")

        except Exception as e:
            print(f"❌ Error: {e}")

        time.sleep(2)

    proc.terminate()
    proc.wait()

if __name__ == "__main__":
    test_fixed_mcp()