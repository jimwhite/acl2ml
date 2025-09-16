
import json
import subprocess
import time
import threading
import queue
import sys

class ProperMCPClient:
    def __init__(self):
        self.server_ready = False

    def wait_for_server_ready(self, process, timeout=30):
        """Wait for MCP server to be ready"""
        start_time = time.time()

        while time.time() - start_time < timeout:
            # Check if we can see MCP-related output
            if process.poll() is None:  # Process is still running
                time.sleep(1)
                # Try sending a test request after some time
                if time.time() - start_time > 10:
                    return True
            else:
                print("Server process terminated unexpectedly")
                return False

        print("Timeout waiting for server")
        return False

    def test_direct_mcp(self):
        """Test MCP server directly"""
        print("🔄 Testing MCP server communication...")

        # Create startup script that includes MCP server
        startup = """
:q
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)
(format t "READY-FOR-MCP\\n")
(force-output)
"""

        try:
            proc = subprocess.Popen(["/home/acl2/saved_acl2"],
                                  stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.STDOUT,
                                  text=True, bufsize=0)

            # Send startup
            proc.stdin.write(startup)
            proc.stdin.flush()

            # Wait for ready signal
            print("Waiting for server ready signal...")

            ready = False
            for _ in range(30):  # 30 second timeout
                line = proc.stdout.readline()
                print(f"Server: {line.strip()}")

                if "READY-FOR-MCP" in line:
                    ready = True
                    break

                time.sleep(1)

            if ready:
                print("✅ Server is ready!")
                return True
            else:
                print("❌ Server not ready")
                return False

        except Exception as e:
            print(f"❌ Error: {e}")
            return False
        finally:
            if proc:
                proc.terminate()

if __name__ == "__main__":
    client = ProperMCPClient()
    client.test_direct_mcp()
