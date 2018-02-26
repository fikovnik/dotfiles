import os
import subprocess

def passwd(acct):
  acct = os.path.basename(acct)
  path = "/home/krikava/.password-store/mail/%s.gpg" % acct
  args = ["gpg", "--use-agent", "--quiet", "--batch", "--for-your-eyes-only", "-d", path]
  try:
    return subprocess.check_output(args).strip()
  except subprocess.CalledProcessError:
    return ""
