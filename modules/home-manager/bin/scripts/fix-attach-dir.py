#!/usr/bin/env python3

import sys
import pathlib

PERSONAL_ATTACH_DIR = "~/org/.attach"
EDUCATION_ATTACH_DIR = "~/org-edu/.attach"

#f758e519-cd49-470d-bbab-aadf5fd946c9
def move_attachment(attachment):
    top_dir = []
    sub_dir = []

    counter = 0
    for char in attachment:
        if counter < 2:
            top_dir.append(char)
            counter += 1
        else:
            sub_dir.append(char)
            counter += 1

    top_dir = "".join(top_dir)
    sub_dir = "".join(sub_dir)

    if not pathlib.Path(EDUCATION_ATTACH_DIR, top_dir).expanduser().exists():
        pathlib.Path(EDUCATION_ATTACH_DIR, top_dir).expanduser().mkdir()

    old_path = pathlib.Path(PERSONAL_ATTACH_DIR, top_dir, sub_dir).expanduser()
    new_path = pathlib.Path(EDUCATION_ATTACH_DIR, top_dir, sub_dir).expanduser()

    old_path.rename(new_path)

if __name__ == "__main__":
    move_attachment(sys.argv[1])
