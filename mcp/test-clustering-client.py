#!/usr/bin/env python3
"""
ACL2(ml) MCP Client - Clustering Demo
Demonstrates clustering functionality on example.lisp definitions
"""

import json
import sys
import subprocess
import time
from pathlib import Path

def send_mcp_request(method, params=None, request_id=1):
    """Send an MCP JSON-RPC request"""
    request = {
        "jsonrpc": "2.0",
        "method": method,
        "id": request_id
    }
    if params:
        request["params"] = params

    return request

def format_request(request):
    """Format request for display"""
    return json.dumps(request, indent=2)

def demo_clustering():
    """Demo ACL2(ml) clustering on example.lisp"""
    print("🧠 ACL2(ml) Clustering Demo")
    print("=" * 50)
    print()

    print("This demo will:")
    print("1. Initialize MCP connection")
    print("2. Show system capabilities")
    print("3. Run clustering on example.lisp definitions")
    print("4. Find similar definitions")
    print("5. Extract ML features from expressions")
    print()

    # Test requests that would be sent to the MCP server
    requests = []

    # 1. Initialize MCP connection
    print("📋 1. MCP Initialization Request:")
    init_req = send_mcp_request("initialize", {
        "protocolVersion": "2024-11-05",
        "capabilities": {"tools": {}},
        "clientInfo": {"name": "ACL2(ml) Clustering Demo", "version": "1.0.0"}
    }, 1)
    requests.append(("Initialize", init_req))
    print(format_request(init_req))
    print()

    # 2. List available tools
    print("📋 2. Tools List Request:")
    tools_req = send_mcp_request("tools/list", request_id=2)
    requests.append(("List Tools", tools_req))
    print(format_request(tools_req))
    print()

    # 3. Demo system capabilities
    print("📋 3. System Demo Request:")
    demo_req = send_mcp_request("tools/call", {
        "name": "acl2ml-demo",
        "arguments": {}
    }, 3)
    requests.append(("System Demo", demo_req))
    print(format_request(demo_req))
    print()

    # 4. Cluster definitions using K-means
    print("📋 4. Cluster Definitions Request (K-means, granularity 3):")
    cluster_req = send_mcp_request("tools/call", {
        "name": "cluster-definitions",
        "arguments": {
            "algorithm": "k",
            "granularity-level": "3"
        }
    }, 4)
    requests.append(("Cluster Definitions", cluster_req))
    print(format_request(cluster_req))
    print()

    # 5. Find similar definitions to THETA_SUM
    print("📋 5. Find Similar Definitions Request (THETA_SUM):")
    similar_req = send_mcp_request("tools/call", {
        "name": "find-similar-definitions",
        "arguments": {
            "target-name": "THETA_SUM",
            "algorithm": "k"
        }
    }, 5)
    requests.append(("Find Similar", similar_req))
    print(format_request(similar_req))
    print()

    # 6. Extract ML features from a complex expression
    print("📋 6. Extract ML Features Request:")
    features_req = send_mcp_request("tools/call", {
        "name": "extract-ml-features",
        "arguments": {
            "expression": "(implies (and (consp x) (natp n)) (equal (append x (reverse y)) (foo x y n)))"
        }
    }, 6)
    requests.append(("Extract Features", features_req))
    print(format_request(features_req))
    print()

    # 7. Analyze a definition structure
    print("📋 7. Analyze Definition Structure Request:")
    analyze_req = send_mcp_request("tools/call", {
        "name": "analyze-definition-structure",
        "arguments": {
            "definition-text": "(defthm append-associative (implies (and (true-listp x) (true-listp y)) (equal (append (append x y) z) (append x (append y z)))))"
        }
    }, 7)
    requests.append(("Analyze Structure", analyze_req))
    print(format_request(analyze_req))
    print()

    # 8. Try EM clustering algorithm
    print("📋 8. EM Clustering Request:")
    em_cluster_req = send_mcp_request("tools/call", {
        "name": "cluster-definitions",
        "arguments": {
            "algorithm": "e",
            "granularity-level": "4"
        }
    }, 8)
    requests.append(("EM Clustering", em_cluster_req))
    print(format_request(em_cluster_req))
    print()

    print("🎯 Expected Results Summary:")
    print("=" * 30)
    print("• Initialize: Server capabilities and version info")
    print("• Tools List: 6 ACL2(ml) tools available")
    print("• System Demo: Status of definitions index and ML features")
    print("• Cluster Definitions: Groups of similar functions by patterns")
    print("• Find Similar: Functions similar to THETA_SUM (THETA_FACT, etc.)")
    print("• Extract Features: Arity-based ML feature vector")
    print("• Analyze Structure: Complete structural analysis with complexity")
    print("• EM Clustering: Alternative clustering using Expectation-Maximization")
    print()

    print("💡 Real MCP Server Integration:")
    print("These requests would be sent to the running ACL2(ml) MCP server.")
    print("The server would process ACL2 definitions using:")
    print("• Original ACL2(ml) feature extraction algorithms")
    print("• K-means, EM, and Farthest-First clustering")
    print("• Similarity search based on structural features")
    print("• Complete integration with ACL2 8.6 theorem prover")
    print()

    return requests

def run_live_demo():
    """Attempt to run against live MCP server if available"""
    print("🚀 Attempting Live Demo...")
    print("=" * 30)

    # Check if MCP server is running
    try:
        # Start the ACL2(ml) MCP server in background
        print("Starting ACL2(ml) MCP server...")
        server_process = subprocess.Popen([
            "/home/acl2/saved_acl2"
        ], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

        # Send the server startup script
        startup_script = """
:q
(load "~/quicklisp/setup.lisp")
(load "/workspaces/acl2ml/mcp/clustering.lisp")
(load "/workspaces/acl2ml/mcp/feature-extraction-full.lisp")
(load "/workspaces/acl2ml/mcp/acl2ml-full-server.lisp")
"""
        server_process.stdin.write(startup_script)
        server_process.stdin.flush()

        print("✓ Server startup initiated")
        print("Note: In a real deployment, the client would communicate")
        print("via JSON-RPC over STDIO with the running MCP server.")

        # Clean up
        server_process.terminate()

    except Exception as e:
        print(f"⚠️  Could not start live server: {e}")
        print("This is expected in the demo environment.")

    print()

if __name__ == "__main__":
    # Run the request demo
    requests = demo_clustering()

    # Show summary
    print("📊 Demo Summary:")
    print(f"Generated {len(requests)} MCP requests")
    print("Each request demonstrates a different ACL2(ml) capability")
    print()

    # Optionally try live demo
    if len(sys.argv) > 1 and sys.argv[1] == "--live":
        run_live_demo()

    print("🎉 ACL2(ml) MCP Clustering Demo Complete!")
    print()
    print("To run with actual MCP server:")
    print("1. Start: /home/acl2/saved_acl2 < acl2ml-full-server.lisp")
    print("2. Connect this client via JSON-RPC over STDIO")
    print("3. Send these exact requests to get clustering results")