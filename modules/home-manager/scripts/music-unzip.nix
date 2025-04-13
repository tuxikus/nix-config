{
  home.file.".local/bin/music-unzip" = {
    text = ''
      #!/usr/bin/env python
      
      from pathlib import Path
      from zipfile import ZipFile
      import unicodedata
      import argparse
      import shutil
      import tempfile
      import re
      
      dl_archive_directory = Path.home() / "multimedia" / "music" / "dls"
      music_directory = Path.home() / "multimedia" / "music"
      
      def get_arguments():
          parser = argparse.ArgumentParser()
          parser.add_argument("-i", "--input", type=str, help="Input zip file", required=True)
      
          return parser.parse_args()
      
      def slugify(input):
          input = unicodedata.normalize("NFKD", input)
          input = input.lower()
          input = re.sub(r"[^\w\s-]", "", input)
          input = re.sub(r"[-\s]+", "-", input).strip("-")
          return input
      
      def main():
          args = get_arguments()
          input_file_path = Path(args.input).resolve()
      
          with tempfile.TemporaryDirectory() as temp_dir:
              # copy, maybe change to rename
              shutil.copy2(input_file_path, temp_dir)
              # unzip
              with ZipFile(f"{temp_dir}/{input_file_path.name}", "r") as zip_ref:
                  zip_ref.extractall(temp_dir)
      
              # get band and album name + suffix (mp3 or flac)
              for current_file in Path(temp_dir).iterdir():
                  if "cover.jpg" in str(current_file) \
                     or "cover.png" in str(current_file) \
                     or "zip" in str(current_file):
                      continue
                  band = slugify(str(current_file.name).split(" - ")[0])
                  album = slugify(str(current_file.name).split(" - ")[1])
                  suffix = current_file.suffix
                  break
      
              if "mp3" in suffix:
                  target_directory = (music_directory / "mp3" / band / album)
                  target_directory.mkdir(parents=True, exist_ok=True)
              elif "flac" in suffix:
                  target_directory = (music_directory / "flac" / band / album)
                  target_directory.mkdir(parents=True, exist_ok=True)
      
              for current_file in Path(temp_dir).iterdir():
                  if ".zip" in str(current_file):
                      continue
                  if "cover.png" in str(current_file) \
                     or "cover.jpg" in str(current_file):
                      shutil.copy2(current_file, target_directory / current_file.name)
                      continue
                  song = slugify(str(current_file.stem).split(" - ")[2])
                  #current_file.rename(target_directory / (song + suffix))
                  shutil.copy2(current_file, target_directory / (song + suffix))
      
          # move zip to archive directory and add the suffix of the containing files before the zip suffix
          # like: "Band - Album.flac.zip" or "Band - Album.mp3.flac"
          shutil.move(input_file_path, dl_archive_directory / (input_file_path.stem + suffix + input_file_path.suffix))
      
      if __name__ == "__main__":
          main()
          print("Done")
      
    '';

    executable = true;
    };
}
