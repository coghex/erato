#!/usr/bin/env python3

import argparse
import re
import subprocess
import sys
from pathlib import Path


def run_command(command: list[str], cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd,
        text=True,
        capture_output=True,
        check=False,
    )


def parse_max_residency_bytes(rts_stderr: str) -> int | None:
    match = re.search(r"([\d,]+)\s+bytes maximum residency", rts_stderr)
    if not match:
        return None
    return int(match.group(1).replace(",", ""))


def format_megabytes(byte_count: int) -> str:
    return f"{byte_count / (1024 * 1024):.1f} MiB"


def main() -> int:
    repo_root = Path(__file__).resolve().parent.parent

    parser = argparse.ArgumentParser(
        description=(
            "Run erato-corpus on a representative fixture and fail if "
            "GHC RTS maximum residency exceeds a threshold."
        )
    )
    parser.add_argument(
        "--file",
        default=str(repo_root / "testtext" / "memory_regression.txt"),
        help="Corpus fixture to run (default: testtext/memory_regression.txt).",
    )
    parser.add_argument(
        "--threshold-mb",
        type=int,
        default=1024,
        help="Maximum allowed RTS residency in MiB (default: 1024).",
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=2,
        help="Parallel jobs passed to erato-corpus (default: 2).",
    )
    parser.add_argument(
        "--show-failures",
        type=int,
        default=1,
        help="Failure sample count passed to erato-corpus (default: 1).",
    )
    parser.add_argument(
        "--max-sentences",
        type=int,
        default=0,
        help="Optional sentence cap; 0 means use the whole fixture (default: 0).",
    )
    parser.add_argument(
        "--skip-build",
        action="store_true",
        help="Skip `cabal build erato-corpus` if the binary is already built.",
    )
    args = parser.parse_args()

    corpus_file = Path(args.file)
    if not corpus_file.is_absolute():
        corpus_file = repo_root / corpus_file
    corpus_file = corpus_file.resolve()

    if not corpus_file.is_file():
        print(f"[error] missing corpus fixture: {corpus_file}", file=sys.stderr)
        return 2

    if not args.skip_build:
        build = run_command(["cabal", "build", "erato-corpus"], repo_root)
        if build.returncode != 0:
            sys.stdout.write(build.stdout)
            sys.stderr.write(build.stderr)
            return build.returncode

    list_bin = run_command(["cabal", "list-bin", "erato-corpus"], repo_root)
    if list_bin.returncode != 0:
        sys.stdout.write(list_bin.stdout)
        sys.stderr.write(list_bin.stderr)
        return list_bin.returncode

    binary_path = list_bin.stdout.strip()
    if not binary_path:
        print("[error] cabal list-bin erato-corpus returned an empty path", file=sys.stderr)
        return 2

    command = [
        binary_path,
        "--file",
        str(corpus_file),
        "--jobs",
        str(args.jobs),
        "--show-failures",
        str(args.show_failures),
    ]
    if args.max_sentences > 0:
        command.extend(["--max-sentences", str(args.max_sentences)])
    command.extend(["+RTS", "-s", "-RTS"])

    run = run_command(command, repo_root)
    sys.stdout.write(run.stdout)
    sys.stderr.write(run.stderr)
    if run.returncode != 0:
        return run.returncode

    residency_bytes = parse_max_residency_bytes(run.stderr)
    if residency_bytes is None:
        print("[error] could not find `bytes maximum residency` in RTS output", file=sys.stderr)
        return 2

    threshold_bytes = args.threshold_mb * 1024 * 1024
    print(
        "[memory-check] maximum residency: "
        f"{format_megabytes(residency_bytes)} "
        f"(threshold: {args.threshold_mb} MiB)"
    )

    if residency_bytes > threshold_bytes:
        print(
            "[memory-check] FAIL: residency exceeded threshold",
            file=sys.stderr,
        )
        return 1

    print("[memory-check] PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
