#lang racket

(require srfi/1)
(require srfi/78)
(provide munge-tag)

;; Although the original pattern uses a large roman numeral, 
;; string-downcase changes it to small.
(define *re*
  '((#rx"\u00b5" . "mu")
    (#rx"\u25b2" . "a")    ; Triangle
    (#rx"\u039b" . "a")    ; Lambda
    (#rx"\u2665" . "heart")
    (#rx"\u00a7" . "section")
    (#rx"\u2206" . "increment")
    (#rx"\u25ca" . "lozenge")
    (#rx"\u2020" . "dagger")
    (#rx"\\^" . "caret")
    (#rx"\u00f0" . "d")
    (#rx"\u00e5" . "a")
    (#rx"\u00ca" . "e")
    (#rx"\u00f3" . "o")
    (#rx"\u00f6" . "o")
    (#rx"\u00fc" . "u")
    (#rx"\u00ef" . "i")
    (#rx"\u00eb" . "e")
    (#rx"\u00e4" . "a")
    (#rx"\u00ff" . "y")
    (#rx"\u00e9" . "e")
    (#rx"\u00e7" . "c")
    (#rx"\u00f1" . "n")
    (#rx" \u2014 " . "-")
    (#rx"\u2212" . "_minus_")      ;; Again, dodgy whitespace usage
    (#rx"\u2192" . "maps_to")      ;; Dodgy usage, but no issues yet
    (#rx"%" . "_percent")
    (#rx"^\\?!$" . "interrobang")
    (#rx"\u0133" . "ij")
    (#rx"://" . "-")
    (#rx"/$"  . "")
    (#rx"\\.$" . "")
    (#rx"\\."    . " ")
    (#rx" -"     . "-")
    (#rx"\u00e1" . "a")
    (#rx"\u00e6" . "ae")
    (#rx"\u00ed" . "i")
    (#rx"\u00f3" . "o")
    (#rx"\u2173" . "iv")
    (#rx"\u2172" . "iii")
    (#rx"\u2171" . "ii")
    (#rx"\u2170" . "i")
    (#px"\\s*/\\s*" . "-")
    (#rx"&" . "and")
    (#rx" \\(" . "-")
    (#rx" +" . "_")
    (#rx"[^a-z0-9_-]" . "")))

(define old car)
(define new cdr)

(define (test)
  (check (munge-tag "µ-Ziq") => "mu-ziq")
  (check (munge-tag "Boy, Boy, Boy")
	 => "boy_boy_boy")
  (check (munge-tag "Live: Mondo, Madrid, ES (2007-05-10)")
	 => "live_mondo_madrid_es-2007-05-10")
  ;; MODIFIER LETTER APOSTROPHE
  (check (munge-tag "97\u02BC Bonnie & Clyde")
	 => "97_bonnie_and_clyde")
  (check (munge-tag "One Drop Of Water/Deep River")
	 => "one_drop_of_water-deep_river")
  ;; ROMAN NUMERAL TWO
  (check (munge-tag "Dead Meat \u2161")
	 => "dead_meat_ii")
  (check (munge-tag "Sigur R\u00f3s")
	 => "sigur_ros")
  (check (munge-tag "\u00cd G\u00e6r")
	 => "i_gaer")
  (check (munge-tag "Star\u00e1lfur")
	 => "staralfur")
  (check (munge-tag "Entheos Audio Archive 1.0")
	 => "entheos_audio_archive_1_0")
  (check (munge-tag "D.C.B.A.-25")
	 => "d_c_b_a-25")
  (check (munge-tag "J.P.P. McStep B. Blues")
	 => "j_p_p_mcstep_b_blues")
  (check (munge-tag "M.O.E.")
	 => "m_o_e")
  (check (munge-tag "Solid Paranoid / The Sense (Remixes)")
	 => "solid_paranoid-the_sense-remixes")
  (check (munge-tag "http://www.residentadvisor.net/")
	 => "http-www_residentadvisor_net")
  (check (munge-tag "Jouw P\u0133n, M\u0133n P\u0133n")
	 => "jouw_pijn_mijn_pijn")
  (check (munge-tag "New General Catalogue \u2160")
	 => "new_general_catalogue_i")
  (check (munge-tag "?!")
	 => "interrobang")
  (check (munge-tag "5%")
	 => "5_percent")
  (check (munge-tag "Le Corps Mince De Fran\u00e7oise")
	 => "le_corps_mince_de_francoise")
  (check (munge-tag "Climbing Up / “Iknimaya — The Path To Heaven”")
	 => "climbing_up-iknimaya-the_path_to_heaven")
  (check (munge-tag "▲NDRΛS")
	 => "andras")
  (check (munge-tag "Ill Niño")
         => "ill_nino"))

(define (munge-tag tag)
  (fold (lambda (x y)
          (regexp-replace* (old x) y (new x)))
        (string-downcase tag)
        *re*))
