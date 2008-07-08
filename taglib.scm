; taglib.scm - interface to taglib c api

(module taglib-2 scheme
  (require scheme/foreign)
  (provide (except-out (all-defined-out) lib))

; <eli> So -- if you use the foreign interface, it is your responsibility
;       to write code that use the librry correctly and avoids all
;       segfaults.
; <eli> In some cases you can't do that -- for example, you provide a raw
;       interface to some library -- in those cases you'd protect your own
;       exported names with an `unsafe!'
  (unsafe!)

  ; TODO: make macro like so
  ;   (define-taglib set-strings-unicode (_bool -> _void))
  ; since a lot of this code is redundant
  ; we also avoid using (all-defined) at the same time

  ; For now, commit to UTF-8 _STRINGs.  Default is the somewhat looser
  ; _string*/utf-8, which we avoid for the moment.
  (default-_string-type _string/utf-8)

  (define lib (ffi-lib "libtag_c.so.0"))


  ; types
  ; XXX: _FILE clashes with the auto-converting string type _FILE from
  ; foreign.ss.  Not sure how to solve this.  Since we don't use _FILE I would
  ; like to just overwrite it, but I don't know how to do this.

  ; Fixed, we can't use _file since that defines file-tag (which we must define
  ; later) to an internal value used in the cstruct mechanism.  Use tl-file
  ; instead.
  (define-cstruct _tl-file            ((dummy _int)))
  (define-cstruct _tag                ((dummy _int)))
  (define-cstruct _audioproperties    ((dummy _int)))
  
  (define _file-type (_enum '(file-mpeg file-ogg-vorbis file-flac file-mpc)))

  ; TODO:
  ;   Prefix import of foreign
  ;   Remove provide all-defined

  (define-syntax define-taglib
    (syntax-rules ()
      ((define-taglib name (t ...))
       (begin
         (provide name)
         (define name
           (get-ffi-obj
            (regexp-replaces 'name
                             '((#rx"-" "_")
                               (#rx"^" "taglib_")))
            lib (_fun t ...)))))))

  ; Now we can just use:
  ;(define-taglib set-strings-unicode (_fun _bool -> _void))

  ; strings
  (define set-strings-unicode
    (get-ffi-obj "taglib_set_strings_unicode" lib
                 (_fun _bool -> _void)))

  (define set-string-management-enabled 
    (get-ffi-obj "taglib_set_string_management_enabled" lib
                 (_fun _bool -> _void)))


  ; file api
  (define file-new
    (get-ffi-obj "taglib_file_new" lib
                 (_fun _path -> _tl-file-pointer)))

  (define file-new-type
    (get-ffi-obj "taglib_file_new_type" lib
                 (_fun _path _file-type -> _tl-file-pointer)))

  (define file-free
    (get-ffi-obj "taglib_file_free" lib
                 (_fun _tl-file-pointer -> _void)))
    
   (define file-tag
     (get-ffi-obj "taglib_file_tag" lib
                  (_fun _tl-file-pointer -> _tag-pointer)))

  (define file-audioproperties
    (get-ffi-obj "taglib_file_audioproperties" lib
                 (_fun _tl-file-pointer -> _audioproperties-pointer)))

  (define file-save
    (get-ffi-obj "taglib_file_save" lib
                 (_fun _tl-file-pointer -> _bool)))
  

  ; tag api
  ; accessors
  (define tag-title
    (get-ffi-obj "taglib_tag_title" lib
                 (_fun _tag-pointer -> _string)))
    
  (define tag-artist
    (get-ffi-obj "taglib_tag_artist" lib
                 (_fun _tag-pointer -> _string)))

  (define tag-album
    (get-ffi-obj "taglib_tag_album" lib
                 (_fun _tag-pointer -> _string)))

  (define tag-comment
    (get-ffi-obj "taglib_tag_comment" lib
                 (_fun _tag-pointer -> _string)))

  (define tag-genre
    (get-ffi-obj "taglib_tag_genre" lib
                 (_fun _tag-pointer -> _string)))

  (define tag-year
    (get-ffi-obj "taglib_tag_year" lib
                 (_fun _tag-pointer -> _uint)))

  (define tag-track
    (get-ffi-obj "taglib_tag_track" lib
                 (_fun _tag-pointer -> _uint)))

  ; modifiers
  (define tag-set-title
    (get-ffi-obj "taglib_tag_set_title" lib
                 (_fun _tag-pointer _string -> _void)))

  (define tag-set-artist
    (get-ffi-obj "taglib_tag_set_artist" lib
                 (_fun _tag-pointer _string -> _void)))

  (define tag-set-album
    (get-ffi-obj "taglib_tag_set_album" lib
                 (_fun _tag-pointer _string -> _void)))

  (define tag-set-comment
    (get-ffi-obj "taglib_tag_set_comment" lib
                 (_fun _tag-pointer _string -> _void)))

  (define tag-set-genre
    (get-ffi-obj "taglib_tag_set_genre" lib
                 (_fun _tag-pointer _string -> _void)))

  (define tag-set-year
    (get-ffi-obj "taglib_tag_set_year" lib
                 (_fun _tag-pointer _uint -> _void)))

  (define tag-set-track
    (get-ffi-obj "taglib_tag_set_track" lib
                 (_fun _tag-pointer _uint -> _void)))

  (define tag-free-strings
    (get-ffi-obj "taglib_tag_free_strings" lib
                 (_fun -> _void)))


  ; audio properties api
  (define audioproperties-length
    (get-ffi-obj "taglib_audioproperties_length" lib
                 (_fun _audioproperties-pointer -> _int)))
    
  (define audioproperties-bitrate
    (get-ffi-obj "taglib_audioproperties_bitrate" lib
                 (_fun _audioproperties-pointer -> _int)))

  (define audioproperties-samplerate
    (get-ffi-obj "taglib_audioproperties_samplerate" lib
                 (_fun _audioproperties-pointer -> _int)))

  (define audioproperties-channels
    (get-ffi-obj "taglib_audioproperties_channels" lib
                 (_fun _audioproperties-pointer -> _int))))


; TODO: write tests with SRFI-78 check.ss
; check explicit renaming macros
