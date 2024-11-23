import subprocess
import argparse

def run_app(port1, port2, camera1, camera2):
    cmd = f"rebar3 shell & rebar3 shell --setcookie port2 --name port2@127.0.0.1"
    subprocess.run(cmd, shell=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run the app on multiple ports with camera inputs")
    parser.add_argument("--port1", type=int, default=8080, help="Port for the first instance")
    parser.add_argument("--port2", type=int, default=8081, help="Port for the second instance")
    parser.add_argument("--camera1", type=str, help="Camera input for the first instance")
    parser.add_argument("--camera2", type=str, help="Camera input for the second instance")
    args = parser.parse_args()

    run_app(args.port1, args.port2, args.camera1, args.camera2)
