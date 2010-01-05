TYPE_CONV_PATH "Papers"

type author = {
  fullname: string;
  institution: string;
  homepage: string option;
}
and conference = {
  short_name: string;
  long_name: string;
  url: string option;
}
and paper = {
  name: string;
  tags: string list;
  authors: author list;
  conference: conference;
  year: int;
  month: string;
  bibtex: string;
  links: (string * string) list; 
  notes: string option;
}
with orm (debug:all)

let _ = 
 ()
