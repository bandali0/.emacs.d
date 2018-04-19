$pdf_previewer = "start zathura %O %S";
$clean_ext = "aux out";
#$pdf_update_method = 4;
#$pdf_update_command = "zathura %O %S";

# Synctex allows one to jump to from the PDF in Zathura to the source in Emacs
# by Ctrl+click in the PDF.
# Tell latexmk to use Zathura as a previewer, and run emacsclient as the Synctex
# editor.
# $pdf_previewer = 'exec zathura --synctex-forward -x \'emacsclient --no-wait +%{line} %{input}\' %O %S';
