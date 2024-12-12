#!/usr/bin/env python3
from pathlib import Path
from zipfile import ZipFile
from mutagen.flac import FLAC
import os
import re
import unicodedata

home_dir = os.path.expanduser("~")
dl_directory = home_dir + "/temp/bandcamp-downloads/"

enable_genre = False
genre = ""
song_count = 0 # used for m3u
album_name = ""
m3u_path = ""
full_band_album_path = ""

def remove_accents(input_str):
    nfkd_form = unicodedata.normalize('NFKD', input_str)
    return ''.join([c for c in nfkd_form if not unicodedata.combining(c)])

# check if dl dir exists
if not os.path.isdir(dl_directory):
    print("Bandcamp download directory does not exist -->", dl_directory)
    exit()

for file in os.listdir(dl_directory):
    if not file.endswith(".zip"):
        continue
    #
    # File from bandcamp: "Band - Album"
    #

    file_without_diacritics = remove_accents(file)

    # split at -
    band_name, split, album_name = file_without_diacritics.partition(" - ")

    print("old band name: ", band_name)
    print("old album name: ", album_name)

    # remove .zip
    album_name = album_name.replace(".zip", "")

    # to lowercase
    band_name = band_name.lower()
    album_name = album_name.lower()

    # replace
    band_name = band_name.replace(" ", "-")
    album_name = album_name.replace(" ", "-")
    band_name = band_name.replace("_", "")
    album_name = album_name.replace("_", "")
    band_name = band_name.replace(".", "")
    album_name = album_name.replace(".", "")
    band_name = band_name.replace(":", "")
    album_name = album_name.replace(":", "")
    band_name = band_name.replace("(", "")
    album_name = album_name.replace("(", "")
    band_name = band_name.replace(")", "")
    album_name = album_name.replace(")", "")
    band_name = band_name.replace("~", "")
    album_name = album_name.replace("~", "")
    band_name = band_name.replace("&", "and")
    album_name = album_name.replace("&", "and")
    band_name = band_name.replace("'", "")
    album_name = album_name.replace("'", "")


    print("new band name: ", band_name)
    print("new album name: ", album_name)

    full_band_album_path = dl_directory + band_name + "/" + album_name

    if enable_genre:
        genre = input("Genre for " + band_name + " --- " + album_name + ": ")

    # create dir
    Path(full_band_album_path).mkdir(parents=True, exist_ok=True)

    old_path = dl_directory + file
    new_path = full_band_album_path + "/" + "album.zip"

    print("moving:", old_path, "to:", new_path)
    os.rename(old_path, new_path)

    # playlist path
    m3u_path = full_band_album_path + "/" + album_name + ".m3u"

    # unzip
    with ZipFile(new_path, "r") as zObject:
        zObject.extractall(path=full_band_album_path)
        zObject.close()

    # delete zip
    os.remove(new_path)

    song_count = 0

    # count flac files
    for current_file in os.listdir(full_band_album_path):
        if current_file.endswith(".flac"):
            song_count += 1

    # create list for
    song_name_list = [None] * song_count

    for flac in os.listdir(full_band_album_path):
        if not flac.endswith(".flac"):
            continue

        old_full_song_path = full_band_album_path + "/" + flac

        # get song name
        split = re.split(r" - \d\d ", flac, maxsplit=1)
        song_name = split[-1]

        # song name to lowercase
        song_name = song_name.lower()

        # replace characters
        song_name = song_name.replace(".flac","")
        song_name = song_name.replace(" ","-")
        song_name = song_name.replace("&","and")
        song_name = song_name.replace(",","")
        song_name = song_name.replace("(","")
        song_name = song_name.replace(")","")
        song_name = song_name.replace("---","-")
        song_name = song_name.replace("--","-")
        song_name = song_name.replace(".","")
        song_name = song_name.replace("`","")
        song_name = song_name.replace("'","")
        song_name = song_name.replace("~","")
        song_name = song_name.replace("!","")
        song_name = song_name.replace("\\","")
        song_name = song_name.replace("/","")

        new_full_song_path = full_band_album_path + "/" + song_name + ".flac"

        os.rename(old_full_song_path, new_full_song_path)

        # flac stuff
        audio = FLAC(new_full_song_path)
        audio["COMMENT"] = ""

        if enable_genre:
            audio["GENRE"] = genre

        track_number = audio["TRACKNUMBER"][0]
        song_name_list[int(track_number) - 1] = song_name + ".flac"

        audio.save()

    with open(m3u_path, "w") as playlist_file:
        for song in song_name_list:
            playlist_file.write(song + "\n")
