{ pkgs, ... }:
let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
	dvisvgm
	dvipng
	ulem
	amsmath;
  });
in
{
  home.packages = with pkgs; [
    tex
  ];
}
