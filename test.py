#!/usr/bin/env python3

import sys
import os
import subprocess
from typing import BinaryIO
from dataclasses import dataclass

CLEOQUA_EXT='.clq'
REC_EXT='.rec'

END_TAG='\n:end:\n'

@dataclass
class TestCase():
  exitcode: int
  stdout: bytes
  stderr: bytes

  def write(self, path: str) -> bool:
    with open(path, 'wb') as f:
      TestCase.write_int_field(f, 'exitcode', self.exitcode)
      TestCase.write_blob_field(f, 'stdout', self.stdout)
      TestCase.write_blob_field(f, 'stderr', self.stderr)
      return True

  @classmethod
  def read(cls, path: str) -> 'TestCase':
    with open(path, 'rb') as f:
      exitcode = TestCase.read_int_field(f, 'exitcode')
      stdout = TestCase.read_blob_field(f, 'stdout')
      stderr = TestCase.read_blob_field(f, 'stderr')
    return cls(exitcode, stdout, stderr)

  @staticmethod
  def write_blob_field(f: BinaryIO, name: str, blob: bytes):
    TestCase.write_int_field(f, name, len(blob))
    f.write(blob)
    f.write(END_TAG.encode())

  @staticmethod
  def write_int_field(f: BinaryIO, name: str, value: int):
    f.write(f':{name} {value}:\n'.encode())

  @staticmethod
  def read_blob_field(f: BinaryIO, name: str) -> bytes:
    line = f.readline()
    field = f':{name} '.encode()
    assert line.startswith(field), f'Broken record file. Missing blob `{name}` tag.'
    assert line.endswith(b':\n'), f'Broken record file. Missing closing tag of blob `{name}`.'
    size = int(line[len(field):-2])
    blob = f.read(size)
    assert f.read(len(END_TAG)) == END_TAG.encode(), f'Broken record file. Missing end of blob `{name}` tag.'
    return blob

  @staticmethod
  def read_int_field(f: BinaryIO, name: str) -> int:
    line = f.readline()
    field = f':{name} '.encode()
    assert line.startswith(field), f'Broken record file. Missing int `{name}` tag.'
    assert line.endswith(b':\n'), f'Broken record file. Missing closing tag of int `{name}`.'
    return int(line[len(field):-2])

def usage():
  print('[INFO]: test.py [OPTIONS] [TEST-DIR]')
  print('[INFO]:   OPTIONS:')
  print('[INFO]:     --help,   -h: Prints this help message.')
  print('[INFO]:     --update, -u: Prints this help message.')


def update_file(path: str):
  print(f'[INFO]: Updating output of `{path}`..')

  asm_path = path[:-len(CLEOQUA_EXT)] + '.S'
  obj_path = path[:-len(CLEOQUA_EXT)] + '.o'
  exe_path = path[:-len(CLEOQUA_EXT)]
  rec_path = path[:-len(CLEOQUA_EXT)] + REC_EXT

  proc = subprocess.run(['./cleoqua', path], capture_output = True)
  if proc.returncode != 0:
    result = proc
  else:
    subprocess.run(['as', '-o', obj_path, asm_path], capture_output = True)
    subprocess.run(['ld', '-o', exe_path, obj_path], capture_output = True)
    result = subprocess.run([exe_path], capture_output = True)

  TestCase(result.returncode, result.stdout.replace(b'\r\n', b'\n'), result.stderr.replace(b'\r\n', b'\n')).write(rec_path)

  print(f'[INFO]: Updated output of `{path}`')

def test_file(path: str):
  print(f'[INFO]: Testing output of `{path}`..')

  asm_path = path[:-len(CLEOQUA_EXT)] + '.S'
  obj_path = path[:-len(CLEOQUA_EXT)] + '.o'
  exe_path = path[:-len(CLEOQUA_EXT)]
  rec_path = path[:-len(CLEOQUA_EXT)] + REC_EXT

  if os.path.exists(rec_path):
    recorded = TestCase.read(rec_path)
  else:
    print(f'[WARN]: Record path for `{path}` not found. Skipping.')
    return

  proc = subprocess.run(['./cleoqua', path], capture_output = True)
  if proc.returncode != 0:
    result = TestCase(proc.returncode, proc.stdout.replace(b'\r\n', b'\n'), proc.stderr.replace(b'\r\n', b'\n'))
  else:
    subprocess.run(['as', '-o', obj_path, asm_path], capture_output = True)
    subprocess.run(['ld', '-o', exe_path, obj_path], capture_output = True)
    proc = subprocess.run([exe_path], capture_output = True)

    result = TestCase(proc.returncode, proc.stdout.replace(b'\r\n', b'\n'), proc.stderr.replace(b'\r\n', b'\n'))

  if recorded == result:
    print(f'[INFO]: `{path}` matched its recorded output.');
  else:
    print(f'[ERR]: Output of `{path}` does not match its recorded output!', file = sys.stderr)
    print(f'[ERR]: Output:', file = sys.stderr)
    print(f'  exit code: {result.exitcode}', file = sys.stderr)
    print(f'  stdout: \n{result.stdout.decode("utf-8")}', file = sys.stderr)
    print(f'  stderr: \n{result.stderr.decode("utf-8")}', file = sys.stderr)
    print(f'[ERR]: Recorded:', file = sys.stderr)
    print(f'  exit code: {recorded.exitcode}', file = sys.stderr)
    print(f'  stdout: \n{recorded.stdout.decode("utf-8")}', file = sys.stderr)
    print(f'  stderr: \n{recorded.stderr.decode("utf-8")}', file = sys.stderr)
    sys.exit(1)

if __name__ == '__main__':
  test_dir = './tests'
  update = False

  i = 0
  for arg in sys.argv:
    if arg[0] == '-':
      arg = arg[1:]
      if arg[0] == '-':
        arg = arg[1:]

      if arg == 'help' or arg == 'h':
        usage()
        sys.exit(0)
      elif arg == 'update' or arg == 'u':
        update = True
      else:
        print(f'[ERR]: Unknown option `{arg}`')
        sys.exit(1)

      continue

    test_dir = arg if i == 1 else test_dir

    i += 1

  print("[INFO]: Building CleoQua..")
  subprocess.run(['rustc', '-o', 'cleoqua', 'cleoqua.rs'], capture_output = True)
  print()

  for root, _, files in os.walk(test_dir):
    for file in filter(lambda path: path.endswith(CLEOQUA_EXT), files):
      file = os.path.join(root, file)
      if update:
        update_file(file)
      else:
        test_file(file)

