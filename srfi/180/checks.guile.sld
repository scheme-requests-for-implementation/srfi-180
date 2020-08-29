(define-library (srfi srfi-180 checks)

  (export i_number_double_huge_neg_exp.json
          i_number_huge_exp.json
          i_number_neg_int_huge_exp.json
          i_number_pos_double_huge_exp.json
          i_number_real_neg_overflow.json
          i_number_real_pos_overflow.json
          i_number_real_underflow.json
          i_number_too_big_neg_int.json
          i_number_too_big_pos_int.json
          i_number_very_big_negative_int.json
          i_object_key_lone_2nd_surrogate.json
          i_string_1st_surrogate_but_2nd_missing.json
          i_string_1st_valid_surrogate_2nd_invalid.json
          i_string_incomplete_surrogate_and_escape_valid.json
          i_string_incomplete_surrogate_pair.json
          i_string_incomplete_surrogates_escape_valid.json
          i_string_invalid_lonely_surrogate.json
          i_string_invalid_surrogate.json
          i_string_invalid_utf-8.json
          i_string_inverted_surrogates_U+1D11E.json
          i_string_iso_latin_1.json
          i_string_lone_second_surrogate.json
          i_string_lone_utf8_continuation_byte.json
          i_string_not_in_unicode_range.json
          i_string_overlong_sequence_2_bytes.json
          i_string_overlong_sequence_6_bytes.json
          i_string_overlong_sequence_6_bytes_null.json
          i_string_truncated-utf-8.json
          i_string_utf16BE_no_BOM.json
          i_string_utf16LE_no_BOM.json
          i_string_UTF-16LE_with_BOM.json
          i_string_UTF-8_invalid_sequence.json
          i_string_UTF8_surrogate_U+D800.json
          i_structure_500_nested_arrays.json
          i_structure_UTF-8_BOM_empty_object.json
          n_boolean_not_true.json
          n_boolean_not_false.json
          n_not_null.json
          n_array_1_true_without_comma.json
          n_array_a_invalid_utf8.json
          n_array_colon_instead_of_comma.json
          n_array_comma_after_close.json
          n_array_comma_and_number.json
          n_array_double_comma.json
          n_array_double_extra_comma.json
          n_array_extra_close.json
          n_array_extra_comma.json
          n_array_incomplete_invalid_value.json
          n_array_incomplete.json
          n_array_inner_array_no_comma.json
          n_array_invalid_utf8.json
          n_array_items_separated_by_semicolon.json
          n_array_just_comma.json
          n_array_just_minus.json
          n_array_missing_value.json
          n_array_newlines_unclosed.json
          n_array_number_and_comma.json
          n_array_number_and_several_commas.json
          n_array_spaces_vertical_tab_formfeed.json
          n_array_star_inside.json
          n_array_unclosed.json
          n_array_unclosed_trailing_comma.json
          n_array_unclosed_with_new_lines.json
          n_array_unclosed_with_object_inside.json
          n_incomplete_false.json
          n_incomplete_null.json
          n_incomplete_true.json
          n_multidigit_number_then_00.json
          n_number_0.1.2.json
          n_number_-01.json
          n_number_0.3e.json
          n_number_0.3e+.json
          n_number_0_capital_E.json
          n_number_0_capital_E+.json
          n_number_0.e1.json
          n_number_0e.json
          n_number_0e+.json
          n_number_1_000.json
          n_number_1.0e-.json
          n_number_1.0e.json
          n_number_1.0e+.json
          n_number_-1.0..json
          n_number_1eE2.json
          n_number_.-1.json
          n_number_+1.json
          n_number_.2e-3.json
          n_number_2.e-3.json
          n_number_2.e+3.json
          n_number_2.e3.json
          n_number_-2..json
          n_number_9.e+.json
          n_number_expression.json
          n_number_hex_1_digit.json
          n_number_hex_2_digits.json
          n_number_infinity.json
          n_number_+Inf.json
          n_number_Inf.json
          n_number_invalid+-.json
          n_number_invalid-negative-real.json
          n_number_invalid-utf-8-in-bigger-int.json
          n_number_invalid-utf-8-in-exponent.json
          n_number_invalid-utf-8-in-int.json
          n_number_++.json
          n_number_minus_infinity.json
          n_number_minus_sign_with_trailing_garbage.json
          n_number_minus_space_1.json
          n_number_-NaN.json
          n_number_NaN.json
          n_number_neg_int_starting_with_zero.json
          n_number_neg_real_without_int_part.json
          n_number_neg_with_garbage_at_end.json
          n_number_real_garbage_after_e.json
          n_number_real_with_invalid_utf8_after_e.json
          n_number_real_without_fractional_part.json
          n_number_starting_with_dot.json
          n_number_U+FF11_fullwidth_digit_one.json
          n_number_with_alpha_char.json
          n_number_with_alpha.json
          n_number_with_leading_zero.json
          n_object_bad_value.json
          n_object_bracket_key.json
          n_object_comma_instead_of_colon.json
          n_object_double_colon.json
          n_object_emoji.json
          n_object_garbage_at_end.json
          n_object_key_with_single_quotes.json
          n_object_lone_continuation_byte_in_key_and_trailing_comma.json
          n_object_missing_colon.json
          n_object_missing_key.json
          n_object_missing_semicolon.json
          n_object_missing_value.json
          n_object_no-colon.json
          n_object_non_string_key_but_huge_number_instead.json
          n_object_non_string_key.json
          n_object_repeated_null_null.json
          n_object_several_trailing_commas.json
          n_object_single_quote.json
          n_object_trailing_comma.json
          n_object_trailing_comment.json
          n_object_trailing_comment_open.json
          n_object_trailing_comment_slash_open_incomplete.json
          n_object_trailing_comment_slash_open.json
          n_object_two_commas_in_a_row.json
          n_object_unquoted_key.json
          n_object_unterminated-value.json
          n_object_with_single_string.json
          n_object_with_trailing_garbage.json
          n_single_space.json
          n_string_1_surrogate_then_escape.json
          n_string_1_surrogate_then_escape_u1.json
          n_string_1_surrogate_then_escape_u1x.json
          n_string_1_surrogate_then_escape_u.json
          n_string_accentuated_char_no_quotes.json
          n_string_backslash_00.json
          n_string_escaped_backslash_bad.json
          n_string_escaped_ctrl_char_tab.json
          n_string_escaped_emoji.json
          n_string_escape_x.json
          n_string_incomplete_escaped_character.json
          n_string_incomplete_escape.json
          n_string_incomplete_surrogate_escape_invalid.json
          n_string_incomplete_surrogate.json
          n_string_invalid_backslash_esc.json
          n_string_invalid_unicode_escape.json
          n_string_invalid_utf8_after_escape.json
          n_string_invalid-utf-8-in-escape.json
          n_string_leading_uescaped_thinspace.json
          n_string_no_quotes_with_bad_escape.json
          n_string_single_doublequote.json
          n_string_single_quote.json
          n_string_single_string_no_double_quotes.json
          n_string_start_escape_unclosed.json
          n_string_unescaped_crtl_char.json
          n_string_unescaped_newline.json
          n_string_unescaped_tab.json
          n_string_unicode_CapitalU.json
          n_string_with_trailing_garbage.json
          n_structure_100000_opening_arrays.json
          n_structure_angle_bracket_..json
          n_structure_angle_bracket_null.json
          n_structure_array_trailing_garbage.json
          n_structure_array_with_extra_array_close.json
          n_structure_array_with_unclosed_string.json
          n_structure_ascii-unicode-identifier.json
          n_structure_capitalized_True.json
          n_structure_close_unopened_array.json
          n_structure_comma_instead_of_closing_brace.json
          n_structure_double_array.json
          n_structure_end_array.json
          n_structure_incomplete_UTF8_BOM.json
          n_structure_lone-invalid-utf-8.json
          n_structure_lone-open-bracket.json
          n_structure_no_data.json
          n_structure_null-byte-outside-string.json
          n_structure_number_with_trailing_garbage.json
          n_structure_object_followed_by_closing_object.json
          n_structure_object_unclosed_no_value.json
          n_structure_object_with_comment.json
          n_structure_object_with_trailing_garbage.json
          n_structure_open_array_apostrophe.json
          n_structure_open_array_comma.json
          n_structure_open_array_object.json
          n_structure_open_array_open_object.json
          n_structure_open_array_open_string.json
          n_structure_open_array_string.json
          n_structure_open_object_close_array.json
          n_structure_open_object_comma.json
          n_structure_open_object.json
          n_structure_open_object_open_array.json
          n_structure_open_object_open_string.json
          n_structure_open_object_string_with_apostrophes.json
          n_structure_open_open.json
          n_structure_single_eacute.json
          n_structure_single_star.json
          n_structure_trailing_sharp.json
          n_structure_U+2060_word_joined.json
          n_structure_uescaped_LF_before_string.json
          n_structure_unclosed_array.json
          n_structure_unclosed_array_partial_null.json
          n_structure_unclosed_array_unfinished_false.json
          n_structure_unclosed_array_unfinished_true.json
          n_structure_unclosed_object.json
          n_structure_unicode-identifier.json
          n_structure_UTF8_BOM_no_data.json
          n_structure_whitespace_formfeed.json
          n_structure_whitespace_U+2060_word_joiner.json
          y_array_arraysWithSpaces.json
          y_array_empty.json
          y_array_empty-string.json
          y_array_ending_with_newline.json
          y_array_false.json
          y_array_heterogeneous.json
          y_array_null.json
          y_array_with_1_and_newline.json
          y_array_with_leading_space.json
          y_array_with_several_null.json
          y_array_with_trailing_space.json
          y_number_0e+1.json
          y_number_0e1.json
          y_number_after_space.json
          y_number_double_close_to_zero.json
          y_number_int_with_exp.json
          y_number.json
          y_number_minus_zero.json
          y_number_negative_int.json
          y_number_negative_one.json
          y_number_negative_zero.json
          y_number_real_capital_e.json
          y_number_real_capital_e_neg_exp.json
          y_number_real_capital_e_pos_exp.json
          y_number_real_exponent.json
          y_number_real_fraction_exponent.json
          y_number_real_neg_exp.json
          y_number_real_pos_exponent.json
          y_number_simple_int.json
          y_number_simple_real.json
          y_object_basic.json
          y_object_duplicated_key_and_value.json
          y_object_duplicated_key.json
          y_object_empty.json
          y_object_empty_key.json
          y_object_escaped_null_in_key.json
          y_object_extreme_numbers.json
          y_object.json
          y_object_long_strings.json
          y_object_simple.json
          y_object_string_unicode.json
          y_object_with_newlines.json
          y_string_1_2_3_bytes_UTF-8_sequences.json
          y_string_accepted_surrogate_pair.json
          y_string_accepted_surrogate_pairs.json
          y_string_allowed_escapes.json
          y_string_backslash_and_u_escaped_zero.json
          y_string_backslash_doublequotes.json
          y_string_comments.json
          y_string_double_escape_a.json
          y_string_double_escape_n.json
          y_string_escaped_control_character.json
          y_string_escaped_noncharacter.json
          y_string_in_array.json
          y_string_in_array_with_leading_space.json
          y_string_last_surrogates_1_and_2.json
          y_string_nbsp_uescaped.json
          y_string_nonCharacterInUTF-8_U+10FFFF.json
          y_string_nonCharacterInUTF-8_U+FFFF.json
          y_string_null_escape.json
          y_string_one-byte-utf-8.json
          y_string_pi.json
          y_string_reservedCharacterInUTF-8_U+1BFFF.json
          y_string_simple_ascii.json
          y_string_space.json
          y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json
          y_string_three-byte-utf-8.json
          y_string_two-byte-utf-8.json
          y_string_u+2028_line_sep.json
          y_string_u+2029_par_sep.json
          y_string_uescaped_newline.json
          y_string_uEscape.json
          y_string_unescaped_char_delete.json
          y_string_unicode_2.json
          y_string_unicodeEscapedBackslash.json
          y_string_unicode_escaped_double_quote.json
          y_string_unicode.json
          y_string_unicode_U+10FFFE_nonchar.json
          y_string_unicode_U+1FFFE_nonchar.json
          y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json
          y_string_unicode_U+2064_invisible_plus.json
          y_string_unicode_U+FDD0_nonchar.json
          y_string_unicode_U+FFFE_nonchar.json
          y_string_utf8.json
          y_string_with_del_character.json
          y_structure_lonely_false.json
          y_structure_lonely_int.json
          y_structure_lonely_negative_real.json
          y_structure_lonely_null.json
          y_structure_lonely_string.json
          y_structure_lonely_true.json
          y_structure_string_empty.json
          y_structure_trailing_newline.json
          y_structure_true_in_array.json
          y_structure_whitespace_array.json
          ;; other tests
          y_object_nested.json
          ;; scheme specific
          n_+inf.0
          n_-inf.0
          n_complex
          n_-nan.0
          n_+nan.0
          n_exact_not_integer
          y_json_lines_numbers
          y_json_lines_arrays
          y_json_lines_objects
          character-limit
          nesting-limit
          parse-into-records
          y_foundationdb_status.json
          sample-crlf-line-separators.jsonl
          sample-no-eol-at-eof.jsonl
          sample.jsonl
          ;; json-sequence
          json-sequence.log
          json-sequence-with-one-broken-json.log
          ;; others
          json-generator-single-top-level-value
          json-generator-single-top-level-value-structure)

  (import (guile))
  (import (scheme base))
  (import (scheme read))
  (import (scheme file))
  (import (srfi srfi-180))
  (import (srfi srfi-158))
  (import (check))

  (begin
    (chdir "./srfi")
    (define (call-with-input-string string proc)
      (call-with-port (open-input-string string) proc))

    (define (call-with-output-string proc)
      (let ((port (open-output-string)))
        (proc port)
        (let ((string (get-output-string port)))
          (close-port port)
          string)))

    (define (json->obj->json->obj filepath)
      (call-with-input-string
       (call-with-output-string
        (lambda (port)
          (json-write (call-with-input-file filepath json-read) port)))
       (lambda (port)
         (json-read port))))

    (define (json-string->obj string)
      (call-with-input-string string json-read))

    (define (obj->json-string obj)
      (call-with-output-string (lambda (port) (json-write obj))))

    (define parse json->obj->json->obj)

    (define i_number_double_huge_neg_exp.json
      (skip check #(0.0) (parse "./files/i_number_double_huge_neg_exp.json")))

    (define i_number_huge_exp.json
      (skip check #(0.4) (parse "./files/i_number_huge_exp.json")))

    (define i_number_neg_int_huge_exp.json
      (skip check-raise json-error? (parse "./files/i_number_neg_int_huge_exp.json")))

    (define i_number_pos_double_huge_exp.json
      (skip check-raise json-error? (parse "./files/i_number_pos_double_huge_exp.json")))

    (define i_number_real_neg_overflow.json
      (skip check-raise json-error? (parse "./files/i_number_real_neg_overflow.json")))

    (define i_number_real_pos_overflow.json
      (skip check-raise json-error? (parse "./files/i_number_real_pos_overflow.json")))

    (define i_number_real_underflow.json
      (skip check #(0.0) (parse "./files/i_number_real_underflow.json")))

    (define i_number_too_big_neg_int.json
      (check #(-123123123123123123123123123123)
             (parse "./files/i_number_too_big_neg_int.json")))

    (define i_number_too_big_pos_int.json
      (check #(100000000000000000000)
             (parse "./files/i_number_too_big_pos_int.json")))

    (define i_number_very_big_negative_int.json
      (check #(-237462374673276894279832749832423479823246327846)
             (parse "./files/i_number_very_big_negative_int.json")))

    (define i_object_key_lone_2nd_surrogate.json
      (skip check '((|���| . 0)) (parse "./files/i_object_key_lone_2nd_surrogate.json")))

    (define i_string_1st_surrogate_but_2nd_missing.json
      (skip check #("���") (parse "./files/i_string_1st_surrogate_but_2nd_missing.json")))

    (define i_string_1st_valid_surrogate_2nd_invalid.json
      (skip check #("���ሴ") (parse "./files/i_string_1st_valid_surrogate_2nd_invalid.json")))

    (define i_string_incomplete_surrogate_and_escape_valid.json
      (skip check #("���\n") (parse "./files/i_string_incomplete_surrogate_and_escape_valid.json")))

    (define i_string_incomplete_surrogate_pair.json
      (skip check #("���a") (parse "./files/i_string_incomplete_surrogate_pair.json")))

    (define i_string_incomplete_surrogates_escape_valid.json
      (skip check #("������\n") (parse "./files/i_string_incomplete_surrogates_escape_valid.json")))

    (define i_string_invalid_lonely_surrogate.json
      (skip check #("���") (parse "./files/i_string_invalid_lonely_surrogate.json")))

    (define i_string_invalid_surrogate.json
      (skip check #("���abc") (parse "./files/i_string_invalid_surrogate.json")))

    (define i_string_invalid_utf-8.json
      (skip check-raise json-error? (parse "./files/i_string_invalid_utf-8.json")))

    (define i_string_inverted_surrogates_U+1D11E.json
      (skip check #("������") (parse "./files/i_string_inverted_surrogates_U+1D11E.json")))

    (define i_string_iso_latin_1.json
      (skip check-raise json-error? (parse "./files/i_string_iso_latin_1.json")))

    (define i_string_lone_second_surrogate.json
      (skip check #("���") (parse "./files/i_string_lone_second_surrogate.json")))

    (define i_string_lone_utf8_continuation_byte.json
      (skip check-raise json-error? (parse "./files/i_string_lone_utf8_continuation_byte.json")))

    (define i_string_not_in_unicode_range.json
      (skip check #("����") (parse "./files/i_string_not_in_unicode_range.json")))

    (define i_string_overlong_sequence_2_bytes.json
      (skip check #("/") (parse "./files/i_string_overlong_sequence_2_bytes.json")))

    (define i_string_overlong_sequence_6_bytes.json
      (skip check-raise json-error? (parse "./files/i_string_overlong_sequence_6_bytes.json")))

    (define i_string_overlong_sequence_6_bytes_null.json
      (skip check-raise json-error? (parse "./files/i_string_overlong_sequence_6_bytes_null.json")))

    (define i_string_truncated-utf-8.json
      (skip check-raise json-error? (parse "./files/i_string_truncated-utf-8.json")))

    ;; XXX: json text must be encoded in utf8?!
    (define i_string_utf16BE_no_BOM.json
      (check-raise json-error? (parse "./files/i_string_utf16BE_no_BOM.json")))

    ;; XXX: json text must be encoded in utf8?!
    (define i_string_utf16LE_no_BOM.json
      (check-raise json-error? (parse "./files/i_string_utf16LE_no_BOM.json")))

    ;; XXX: json text must be encoded in utf8?!
    (define i_string_UTF-16LE_with_BOM.json
      (check-raise json-error? (parse "./files/i_string_UTF-16LE_with_BOM.json")))

    (define i_string_UTF-8_invalid_sequence.json
      (skip check-raise json-error? (parse "./files/i_string_UTF-8_invalid_sequence.json")))

    ;; XXX: accepted but returns garbage
    (define i_string_UTF8_surrogate_U+D800.json
      (skip check #("���") (parse "./files/i_string_UTF8_surrogate_U+D800.json")))

    ;; TODO: convert this giant array of array into a let loop
    (define i_structure_500_nested_arrays.json
      (check #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
             (parse "./files/i_structure_500_nested_arrays.json")))

    ;; it seems to me BOM must not be part of JSON text
    (define i_structure_UTF-8_BOM_empty_object.json
      (check '() (parse "./files/i_structure_UTF-8_BOM_empty_object.json")))

    (define n_boolean_not_true.json
      (check-raise json-error? (parse "./files/n_boolean_not_true.json")))

    (define n_boolean_not_false.json
      (check-raise json-error? (parse "./files/n_boolean_not_false.json")))

    (define n_not_null.json
      (check-raise json-error? (parse "./files/n_not_null.json")))

    (define n_array_1_true_without_comma.json
      (check-raise json-error? (parse "./files/n_array_1_true_without_comma.json")))

    (define n_array_a_invalid_utf8.json
      (check-raise json-error? (parse "./files/n_array_a_invalid_utf8.json")))

    (define n_array_colon_instead_of_comma.json
      (check-raise json-error? (parse "./files/n_array_colon_instead_of_comma.json")))

    (define n_array_comma_after_close.json
      ;; The parser read a single JSON toplevel value, and ignore the
      ;; rest.
      (skip check-raise json-error? (parse "./files/n_array_comma_after_close.json")))

    (define n_array_comma_and_number.json
      (check-raise json-error? (parse "./files/n_array_comma_and_number.json")))

    (define n_array_double_comma.json
      (check-raise json-error? (parse "./files/n_array_double_comma.json")))

    (define n_array_double_extra_comma.json
      (check-raise json-error? (parse "./files/n_array_double_extra_comma.json")))

    (define n_array_extra_close.json
      ;; XXX: The parser reads a single toplevel JSON value, and
      ;; ignore the rest.
      (skip check-raise json-error? (parse "./files/n_array_extra_close.json")))

    (define n_array_extra_comma.json
      (check-raise json-error? (parse "./files/n_array_extra_comma.json")))

    (define n_array_incomplete_invalid_value.json
      (check-raise json-error? (parse "./files/n_array_incomplete_invalid_value.json")))

    (define n_array_incomplete.json
      (check-raise json-error? (parse "./files/n_array_incomplete.json")))

    (define n_array_inner_array_no_comma.json
      (check-raise json-error? (parse "./files/n_array_inner_array_no_comma.json")))

    ;; TODO: investigate
    (define n_array_invalid_utf8.json
      (check-raise json-error? (parse "./files/n_array_invalid_utf8.json")))

    (define n_array_items_separated_by_semicolon.json
      (check-raise json-error? (parse "./files/n_array_items_separated_by_semicolon.json")))

    (define n_array_just_comma.json
      (check-raise json-error? (parse "./files/n_array_just_comma.json")))

    (define n_array_just_minus.json
      (check-raise json-error? (parse "./files/n_array_just_minus.json")))

    (define n_array_missing_value.json
      (check-raise json-error? (parse "./files/n_array_missing_value.json")))

    (define n_array_newlines_unclosed.json
      (check-raise json-error? (parse "./files/n_array_newlines_unclosed.json")))

    (define n_array_number_and_comma.json
      (check-raise json-error? (parse "./files/n_array_number_and_comma.json")))

    (define n_array_number_and_several_commas.json
      (check-raise json-error? (parse "./files/n_array_number_and_several_commas.json")))

    (define n_array_spaces_vertical_tab_formfeed.json
      (check-raise json-error? (parse "./files/n_array_spaces_vertical_tab_formfeed.json")))

    (define n_array_star_inside.json
      (check-raise json-error? (parse "./files/n_array_star_inside.json")))

    (define n_array_unclosed.json
      (check-raise json-error? (parse "./files/n_array_unclosed.json")))

    (define n_array_unclosed_trailing_comma.json
      (check-raise json-error? (parse "./files/n_array_unclosed_trailing_comma.json")))

    (define n_array_unclosed_with_new_lines.json
      (check-raise json-error? (parse "./files/n_array_unclosed_with_new_lines.json")))

    (define n_array_unclosed_with_object_inside.json
      (check-raise json-error? (parse "./files/n_array_unclosed_with_object_inside.json")))

    (define n_incomplete_false.json
      (check-raise json-error? (parse "./files/n_incomplete_false.json")))

    (define n_incomplete_null.json
      (check-raise json-error? (parse "./files/n_incomplete_null.json")))

    (define n_incomplete_true.json
      (check-raise json-error? (parse "./files/n_incomplete_true.json")))

    (define n_multidigit_number_then_00.json
      (check-raise json-error? (parse "./files/n_multidigit_number_then_00.json")))

    (define n_number_0.1.2.json
      (check-raise json-error? (parse "./files/n_number_0.1.2.json")))

    ;; XXX: harmless but not standard
    (define n_number_-01.json
      (skip check-raise json-error? (parse "./files/n_number_-01.json")))

    (define n_number_0.3e.json
      (check-raise json-error? (parse "./files/n_number_0.3e.json")))

    (define n_number_0.3e+.json
      (check-raise json-error? (parse "./files/n_number_0.3e+.json")))

    (define n_number_0_capital_E.json
      (check-raise json-error? (parse "./files/n_number_0_capital_E.json")))

    (define n_number_0_capital_E+.json
      (check-raise json-error? (parse "./files/n_number_0_capital_E+.json")))

    ;; XXX: harmless but not standard
    (define n_number_0.e1.json
      (skip check-raise json-error? (parse "./files/n_number_0.e1.json")))

    (define n_number_0e.json
      (check-raise json-error? (parse "./files/n_number_0e.json")))

    (define n_number_0e+.json
      (check-raise json-error? (parse "./files/n_number_0e+.json")))

    (define n_number_1_000.json
      (check-raise json-error? (parse "./files/n_number_1_000.json")))

    (define n_number_1.0e-.json
      (check-raise json-error? (parse "./files/n_number_1.0e-.json")))

    (define n_number_1.0e.json
      (check-raise json-error? (parse "./files/n_number_1.0e.json")))

    (define n_number_1.0e+.json
      (check-raise json-error? (parse "./files/n_number_1.0e+.json")))

    (define n_number_-1.0..json
      (check-raise json-error? (parse "./files/n_number_-1.0..json")))

    ;; XXX: harmless but not standard
    (define n_number_1eE2.json
      (check-raise json-error? (parse "./files/n_number_1eE2.json")))

    (define n_number_.-1.json
      (check-raise json-error? (parse "./files/n_number_.-1.json")))

    (define n_number_+1.json
      (skip check-raise json-error? (parse "./files/n_number_+1.json")))

    ;; XXX: harmless but not standard
    (define n_number_.2e-3.json
      (skip check-raise json-error? (parse "./files/n_number_.2e-3.json")))

    ;; XXX: harmless but not standard
    (define n_number_2.e-3.json
      (skip check-raise json-error? (parse "./files/n_number_2.e-3.json")))

    ;; XXX: harmless but not standard
    (define n_number_2.e+3.json
      (skip check-raise json-error? (parse "./files/n_number_2.e+3.json")))

    ;; XXX: harmless but not standard
    (define n_number_2.e3.json
      (skip check-raise json-error? (parse "./files/n_number_2.e3.json")))

    ;; XXX: harmless but not standard
    (define n_number_-2..json
      (skip check-raise json-error? (parse "./files/n_number_-2..json")))

    (define n_number_9.e+.json
      (check-raise json-error? (parse "./files/n_number_9.e+.json")))

    (define n_number_expression.json
      (check-raise json-error? (parse "./files/n_number_expression.json")))

    (define n_number_hex_1_digit.json
      (check-raise json-error? (parse "./files/n_number_hex_1_digit.json")))

    (define n_number_hex_2_digits.json
      (check-raise json-error? (parse "./files/n_number_hex_2_digits.json")))

    (define n_number_infinity.json
      (check-raise json-error? (parse "./files/n_number_infinity.json")))

    (define n_number_+Inf.json
      (check-raise json-error? (parse "./files/n_number_+Inf.json")))

    (define n_number_Inf.json
      (check-raise json-error? (parse "./files/n_number_Inf.json")))

    ;; XXX: harmless but not standard
    (define n_number_invalid+-.json
      (check-raise json-error? (parse "./files/n_number_invalid+-.json")))

    (define n_number_invalid-negative-real.json
      (check-raise json-error? (parse "./files/n_number_invalid-negative-real.json")))

    (define n_number_invalid-utf-8-in-bigger-int.json
      (check-raise json-error? (parse "./files/n_number_invalid-utf-8-in-bigger-int.json")))

    (define n_number_invalid-utf-8-in-exponent.json
      (check-raise json-error? (parse "./files/n_number_invalid-utf-8-in-exponent.json")))

    (define n_number_invalid-utf-8-in-int.json
      (check-raise json-error? (parse "./files/n_number_invalid-utf-8-in-int.json")))

    (define n_number_++.json
      (check-raise json-error? (parse "./files/n_number_++.json")))

    (define n_number_minus_infinity.json
      (check-raise json-error? (parse "./files/n_number_minus_infinity.json")))

    (define n_number_minus_sign_with_trailing_garbage.json
      (check-raise json-error? (parse "./files/n_number_minus_sign_with_trailing_garbage.json")))

    (define n_number_minus_space_1.json
      (check-raise json-error? (parse "./files/n_number_minus_space_1.json")))

    (define n_number_-NaN.json
      (check-raise json-error? (parse "./files/n_number_-NaN.json")))

    (define n_number_NaN.json
      (check-raise json-error? (parse "./files/n_number_NaN.json")))

    ;; XXX: harmless but not standard
    (define n_number_neg_int_starting_with_zero.json
      (skip check-raise json-error? (parse "./files/n_number_neg_int_starting_with_zero.json")))

    (define n_number_neg_real_without_int_part.json
      (skip check-raise json-error? (parse "./files/n_number_neg_real_without_int_part.json")))

    (define n_number_neg_with_garbage_at_end.json
      (check-raise json-error? (parse "./files/n_number_neg_with_garbage_at_end.json")))

    (define n_number_real_garbage_after_e.json
      (check-raise json-error? (parse "./files/n_number_real_garbage_after_e.json")))

    (define n_number_real_with_invalid_utf8_after_e.json
      (check-raise json-error? (parse "./files/n_number_real_with_invalid_utf8_after_e.json")))

    (define n_number_real_without_fractional_part.json
      (skip check-raise json-error? (parse "./files/n_number_real_without_fractional_part.json")))

    (define n_number_starting_with_dot.json
      (skip check-raise json-error? (parse "./files/n_number_starting_with_dot.json")))

    (define n_number_U+FF11_fullwidth_digit_one.json
      (check-raise json-error? (parse "./files/n_number_U+FF11_fullwidth_digit_one.json")))

    (define n_number_with_alpha_char.json
      (check-raise json-error? (parse "./files/n_number_with_alpha_char.json")))

    (define n_number_with_alpha.json
      (check-raise json-error? (parse "./files/n_number_with_alpha.json")))

    (define n_number_with_leading_zero.json
      (skip check-raise json-error? (parse "./files/n_number_with_leading_zero.json")))

    (define n_object_bad_value.json
      (check-raise json-error? (parse "./files/n_object_bad_value.json")))

    (define n_object_bracket_key.json
      (check-raise json-error? (parse "./files/n_object_bracket_key.json")))

    (define n_object_comma_instead_of_colon.json
      (check-raise json-error? (parse "./files/n_object_comma_instead_of_colon.json")))

    (define n_object_double_colon.json
      (check-raise json-error? (parse "./files/n_object_double_colon.json")))

    (define n_object_emoji.json
      (check-raise json-error? (parse "./files/n_object_emoji.json")))

    (define n_object_garbage_at_end.json
      (check-raise json-error? (parse "./files/n_object_garbage_at_end.json")))

    (define n_object_key_with_single_quotes.json
      (check-raise json-error? (parse "./files/n_object_key_with_single_quotes.json")))

    (define n_object_lone_continuation_byte_in_key_and_trailing_comma.json
      (check-raise json-error? (parse "./files/n_object_lone_continuation_byte_in_key_and_trailing_comma.json")))

    (define n_object_missing_colon.json
      (check-raise json-error? (parse "./files/n_object_missing_colon.json")))

    (define n_object_missing_key.json
      (check-raise json-error? (parse "./files/n_object_missing_key.json")))

    (define n_object_missing_semicolon.json
      (check-raise json-error? (parse "./files/n_object_missing_semicolon.json")))

    (define n_object_missing_value.json
      (check-raise json-error? (parse "./files/n_object_missing_value.json")))

    (define n_object_no-colon.json
      (check-raise json-error? (parse "./files/n_object_no-colon.json")))

    (define n_object_non_string_key_but_huge_number_instead.json
      (skip check-raise json-error? (parse "./files/n_object_non_string_key_but_huge_number_instead.json")))

    (define n_object_non_string_key.json
      (check-raise json-error? (parse "./files/n_object_non_string_key.json")))

    (define n_object_repeated_null_null.json
      (check-raise json-error? (parse "./files/n_object_repeated_null_null.json")))

    (define n_object_several_trailing_commas.json
      (check-raise json-error? (parse "./files/n_object_several_trailing_commas.json")))

    (define n_object_single_quote.json
      (check-raise json-error? (parse "./files/n_object_single_quote.json")))

    (define n_object_trailing_comma.json
      (check-raise json-error? (parse "./files/n_object_trailing_comma.json")))

    (define n_object_trailing_comment.json
      ;; XXX: The parser read a single toplevel JSON value, and ignore
      ;; the rest.
      (skip check-raise json-error? (parse "./files/n_object_trailing_comment.json")))

    (define n_object_trailing_comment_open.json
      ;; XXX: The parser read a single toplevel JSON value, and ignore
      ;; the rest.
      (skip check-raise json-error? (parse "./files/n_object_trailing_comment_open.json")))

    (define n_object_trailing_comment_slash_open_incomplete.json
      ;; XXX: The parser read a single toplevel JSON value, and ignore the rest.
      (skip check-raise json-error? (parse "./files/n_object_trailing_comment_slash_open_incomplete.json")))

    (define n_object_trailing_comment_slash_open.json
      ;; XXX: The parser read a single toplevel JSON value, and ignore the rest.
      (skip check-raise json-error? (parse "./files/n_object_trailing_comment_slash_open.json")))

    (define n_object_two_commas_in_a_row.json
      (check-raise json-error? (parse "./files/n_object_two_commas_in_a_row.json")))

    (define n_object_unquoted_key.json
      (check-raise json-error? (parse "./files/n_object_unquoted_key.json")))

    (define n_object_unterminated-value.json
      (check-raise json-error? (parse "./files/n_object_unterminated-value.json")))

    (define n_object_with_single_string.json
      (check-raise json-error? (parse "./files/n_object_with_single_string.json")))

    (define n_object_with_trailing_garbage.json
      ;; XXX: The parser read a single toplevel value, and ignore the
      ;; rest.
      (skip check-raise json-error? (parse "./files/n_object_with_trailing_garbage.json")))

    (define n_single_space.json
      (check-raise json-error? (parse "./files/n_single_space.json")))

    (define n_string_1_surrogate_then_escape.json
      (skip check-raise json-error? (parse "./files/n_string_1_surrogate_then_escape.json")))

    (define n_string_1_surrogate_then_escape_u1.json
      (check-raise json-error? (parse "./files/n_string_1_surrogate_then_escape_u1.json")))

    (define n_string_1_surrogate_then_escape_u1x.json
      (check-raise json-error? (parse "./files/n_string_1_surrogate_then_escape_u1x.json")))

    (define n_string_1_surrogate_then_escape_u.json
      (check-raise json-error? (parse "./files/n_string_1_surrogate_then_escape_u.json")))

    (define n_string_accentuated_char_no_quotes.json
      (check-raise json-error? (parse "./files/n_string_accentuated_char_no_quotes.json")))

    (define n_string_backslash_00.json
      (check-raise json-error? (parse "./files/n_string_backslash_00.json")))

    (define n_string_escaped_backslash_bad.json
      (check-raise json-error? (parse "./files/n_string_escaped_backslash_bad.json")))

    (define n_string_escaped_ctrl_char_tab.json
      (check-raise json-error? (parse "./files/n_string_escaped_ctrl_char_tab.json")))

    (define n_string_escaped_emoji.json
      (check-raise json-error? (parse "./files/n_string_escaped_emoji.json")))

    (define n_string_escape_x.json
      (check-raise json-error? (parse "./files/n_string_escape_x.json")))

    (define n_string_incomplete_escaped_character.json
      (check-raise json-error? (parse "./files/n_string_incomplete_escaped_character.json")))

    (define n_string_incomplete_escape.json
      (check-raise json-error? (parse "./files/n_string_incomplete_escape.json")))

    (define n_string_incomplete_surrogate_escape_invalid.json
      (check-raise json-error? (parse "./files/n_string_incomplete_surrogate_escape_invalid.json")))

    (define n_string_incomplete_surrogate.json
      (check-raise json-error? (parse "./files/n_string_incomplete_surrogate.json")))

    (define n_string_invalid_backslash_esc.json
      (check-raise json-error? (parse "./files/n_string_invalid_backslash_esc.json")))

    (define n_string_invalid_unicode_escape.json
      (check-raise json-error? (parse "./files/n_string_invalid_unicode_escape.json")))

    (define n_string_invalid_utf8_after_escape.json
      (check-raise json-error? (parse "./files/n_string_invalid_utf8_after_escape.json")))

    (define n_string_invalid-utf-8-in-escape.json
      (check-raise json-error? (parse "./files/n_string_invalid-utf-8-in-escape.json")))

    (define n_string_leading_uescaped_thinspace.json
      (check-raise json-error? (parse "./files/n_string_leading_uescaped_thinspace.json")))

    (define n_string_no_quotes_with_bad_escape.json
      (check-raise json-error? (parse "./files/n_string_no_quotes_with_bad_escape.json")))

    (define n_string_single_doublequote.json
      (check-raise json-error? (parse "./files/n_string_single_doublequote.json")))

    (define n_string_single_quote.json
      (check-raise json-error? (parse "./files/n_string_single_quote.json")))

    (define n_string_single_string_no_double_quotes.json
      (check-raise json-error? (parse "./files/n_string_single_string_no_double_quotes.json")))

    (define n_string_start_escape_unclosed.json
      (check-raise json-error? (parse "./files/n_string_start_escape_unclosed.json")))

    (define n_string_unescaped_crtl_char.json
      (check-raise json-error? (parse "./files/n_string_unescaped_crtl_char.json")))

    (define n_string_unescaped_newline.json
      (check-raise json-error? (parse "./files/n_string_unescaped_newline.json")))

    (define n_string_unescaped_tab.json
      (check-raise json-error? (parse "./files/n_string_unescaped_tab.json")))

    (define n_string_unicode_CapitalU.json
      (check-raise json-error? (parse "./files/n_string_unicode_CapitalU.json")))

    (define n_string_with_trailing_garbage.json
      ;; The parser read a single toplevel value, and ignore the rest.
      (skip check-raise json-error? (parse "./files/n_string_with_trailing_garbage.json")))

    (define n_structure_100000_opening_arrays.json
      ;; TODO: unskip when limit is here
      (skip check-raise json-error? (parse "./files/n_structure_100000_opening_arrays.json")))

    (define n_structure_angle_bracket_..json
      (check-raise json-error? (parse "./files/n_structure_angle_bracket_..json")))

    (define n_structure_angle_bracket_null.json
      (check-raise json-error? (parse "./files/n_structure_angle_bracket_null.json")))

    (define n_structure_array_trailing_garbage.json
      ;; XXX: The parser reads a single JSON toplevel value and ignore
      ;; the rest.
      (skip check-raise json-error? (parse "./files/n_structure_array_trailing_garbage.json")))

    (define n_structure_array_with_extra_array_close.json
      ;; XXX: The parser consider a single toplevel value.
      (skip check-raise json-error? (parse "./files/n_structure_array_with_extra_array_close.json")))

    (define n_structure_array_with_unclosed_string.json
      (check-raise json-error? (parse "./files/n_structure_array_with_unclosed_string.json")))

    (define n_structure_ascii-unicode-identifier.json
      (check-raise json-error? (parse "./files/n_structure_ascii-unicode-identifier.json")))

    (define n_structure_capitalized_True.json
      (check-raise json-error? (parse "./files/n_structure_capitalized_True.json")))

    (define n_structure_close_unopened_array.json
      ;; XXX: The parser reads a single toplevel value, and ignore the
      ;; rest.
      (skip check-raise json-error? (parse "./files/n_structure_close_unopened_array.json")))

    (define n_structure_comma_instead_of_closing_brace.json
      (check-raise json-error? (parse "./files/n_structure_comma_instead_of_closing_brace.json")))

    (define n_structure_double_array.json
      ;; XXX: The parser considers a single JSON toplevel value
      (skip check-raise json-error? (parse "./files/n_structure_double_array.json")))

    (define n_structure_end_array.json
      (check-raise json-error? (parse "./files/n_structure_end_array.json")))

    (define n_structure_incomplete_UTF8_BOM.json
      (check-raise json-error? (parse "./files/n_structure_incomplete_UTF8_BOM.json")))

    (define n_structure_lone-invalid-utf-8.json
      (check-raise json-error? (parse "./files/n_structure_lone-invalid-utf-8.json")))

    (define n_structure_lone-open-bracket.json
      (check-raise json-error? (parse "./files/n_structure_lone-open-bracket.json")))

    (define n_structure_no_data.json
      (check-raise json-error? (parse "./files/n_structure_no_data.json")))

    (define n_structure_null-byte-outside-string.json
      (check-raise json-error? (parse "./files/n_structure_null-byte-outside-string.json")))

    (define n_structure_number_with_trailing_garbage.json
      ;; XXX: The parser read a single toplevel value.
      (skip check-raise json-error? (parse "./files/n_structure_number_with_trailing_garbage.json")))

    (define n_structure_object_followed_by_closing_object.json
      ;; XXX: The parser reads a single toplevel value, and will not
      ;; consider the rest of the text, until another json-read is
      ;; done.
      (skip check-raise json-error? (parse "./files/n_structure_object_followed_by_closing_object.json")))

    (define n_structure_object_unclosed_no_value.json
      (check-raise json-error? (parse "./files/n_structure_object_unclosed_no_value.json")))

    (define n_structure_object_with_comment.json
      (check-raise json-error? (parse "./files/n_structure_object_with_comment.json")))

    (define n_structure_object_with_trailing_garbage.json
      ;; XXX: The parser will read a single top level JSON value and
      ;; return it.  It will not consider the whole string.
      (skip check-raise json-error? (parse "./files/n_structure_object_with_trailing_garbage.json")))

    (define n_structure_open_array_apostrophe.json
      (check-raise json-error? (parse "./files/n_structure_open_array_apostrophe.json")))

    (define n_structure_open_array_comma.json
      (check-raise json-error? (parse "./files/n_structure_open_array_comma.json")))

    (define n_structure_open_array_object.json
      ;; TODO: unskip once there is a paramter json-max-nesting-level
      (skip check-raise json-error? (parse "./files/n_structure_open_array_object.json")))

    (define n_structure_open_array_open_object.json
      (check-raise json-error? (parse "./files/n_structure_open_array_open_object.json")))

    (define n_structure_open_array_open_string.json
      (check-raise json-error? (parse "./files/n_structure_open_array_open_string.json")))

    (define n_structure_open_array_string.json
      (check-raise json-error? (parse "./files/n_structure_open_array_string.json")))

    (define n_structure_open_object_close_array.json
      (check-raise json-error? (parse "./files/n_structure_open_object_close_array.json")))

    (define n_structure_open_object_comma.json
      (check-raise json-error? (parse "./files/n_structure_open_object_comma.json")))

    (define n_structure_open_object.json
      (check-raise json-error? (parse "./files/n_structure_open_object.json")))

    (define n_structure_open_object_open_array.json
      (check-raise json-error? (parse "./files/n_structure_open_object_open_array.json")))

    (define n_structure_open_object_open_string.json
      (check-raise json-error? (parse "./files/n_structure_open_object_open_string.json")))

    (define n_structure_open_object_string_with_apostrophes.json
      (check-raise json-error? (parse "./files/n_structure_open_object_string_with_apostrophes.json")))

    (define n_structure_open_open.json
      (check-raise json-error? (parse "./files/n_structure_open_open.json")))

    (define n_structure_single_eacute.json
      (check-raise json-error? (parse "./files/n_structure_single_eacute.json")))

    (define n_structure_single_star.json
      (check-raise json-error? (parse "./files/n_structure_single_star.json")))

    (define n_structure_trailing_sharp.json
      ;; XXX: the parser will read the first JSON and stop there, if
      ;; there is more characters after a JSON sequence, it will not
      ;; be taken in to account. That is, what follows a JSON text
      ;; does matter, as long as there is proper object / array that
      ;; open / close and string double quotes and escapes.
      (skip check-raise json-error? (parse "./files/n_structure_trailing_#.json")))

    (define n_structure_U+2060_word_joined.json
      (check-raise json-error? (parse "./files/n_structure_U+2060_word_joined.json")))

    (define n_structure_uescaped_LF_before_string.json
      (check-raise json-error? (parse "./files/n_structure_uescaped_LF_before_string.json")))

    (define n_structure_unclosed_array.json
      (check-raise json-error? (parse "./files/n_structure_unclosed_array.json")))

    (define n_structure_unclosed_array_partial_null.json
      (check-raise json-error? (parse "./files/n_structure_unclosed_array_partial_null.json")))

    (define n_structure_unclosed_array_unfinished_false.json
      (check-raise json-error? (parse "./files/n_structure_unclosed_array_unfinished_false.json")))

    (define n_structure_unclosed_array_unfinished_true.json
      (check-raise json-error? (parse "./files/n_structure_unclosed_array_unfinished_true.json")))

    (define n_structure_unclosed_object.json
      (check-raise json-error? (parse "./files/n_structure_unclosed_object.json")))

    (define n_structure_unicode-identifier.json
      (check-raise json-error? (parse "./files/n_structure_unicode-identifier.json")))

    (define n_structure_UTF8_BOM_no_data.json
      (check-raise json-error? (parse "./files/n_structure_UTF8_BOM_no_data.json")))

    (define n_structure_whitespace_formfeed.json
      (check-raise json-error? (parse "./files/n_structure_whitespace_formfeed.json")))

    ;; TODO: FIXME
    (define n_structure_whitespace_U+2060_word_joiner.json
      (check-raise json-error? (parse "./files/n_structure_whitespace_U+2060_word_joiner.json")))

    (define y_array_arraysWithSpaces.json
      (check #(#()) (parse "./files/y_array_arraysWithSpaces.json")))

    (define y_array_empty.json
      (check #() (parse "./files/y_array_empty.json")))

    (define y_array_empty-string.json
      (check #("") (parse "./files/y_array_empty-string.json")))

    (define y_array_ending_with_newline.json
      (check #("a") (parse "./files/y_array_ending_with_newline.json")))

    (define y_array_false.json
      (check #(#f) (parse "./files/y_array_false.json")))

    (define y_array_heterogeneous.json
      (check #(null 1 "1" ()) (parse "./files/y_array_heterogeneous.json")))

    (define y_array_null.json
      (check #(null) (parse "./files/y_array_null.json")))

    (define y_array_with_1_and_newline.json
      (check #(1) (parse "./files/y_array_with_1_and_newline.json")))

    (define y_array_with_leading_space.json
      (check #(1) (parse "./files/y_array_with_leading_space.json")))

    (define y_array_with_several_null.json
      (check #(1 null null null 2) (parse "./files/y_array_with_several_null.json")))

    (define y_array_with_trailing_space.json
      (check #(2) (parse "./files/y_array_with_trailing_space.json")))

    (define y_number_0e+1.json
      (check #(0.0) (parse "./files/y_number_0e+1.json")))

    (define y_number_0e1.json
      (check #(0.0) (parse "./files/y_number_0e1.json")))

    (define y_number_after_space.json
      (check #(4) (parse "./files/y_number_after_space.json")))

    (define y_number_double_close_to_zero.json
      (check #(-1e-78) (parse "./files/y_number_double_close_to_zero.json")))

    (define y_number_int_with_exp.json
      (check #(200.0) (parse "./files/y_number_int_with_exp.json")))

    ;; XXX: not determinist
    (define y_number.json
      (check #(1.23e+67) (parse "./files/y_number.json")))

    (define y_number_minus_zero.json
      (check #(0) (parse "./files/y_number_minus_zero.json")))

    (define y_number_negative_int.json
      (check #(-123) (parse "./files/y_number_negative_int.json")))

    (define y_number_negative_one.json
      (check #(-1) (parse "./files/y_number_negative_one.json")))

    (define y_number_negative_zero.json
      (check #(0) (parse "./files/y_number_negative_zero.json")))

    (define y_number_real_capital_e.json
      (check #(1e+22) (parse "./files/y_number_real_capital_e.json")))

    (define y_number_real_capital_e_neg_exp.json
      (check #(0.01) (parse "./files/y_number_real_capital_e_neg_exp.json")))

    (define y_number_real_capital_e_pos_exp.json
      (check #(100.0) (parse "./files/y_number_real_capital_e_pos_exp.json")))

    ;; XXX: not determinist.
    (define y_number_real_exponent.json
      (skip check #(1.23e+47) (parse "./files/y_number_real_exponent.json")))

    (define y_number_real_fraction_exponent.json
      (check #(1.23456e+80) (parse "./files/y_number_real_fraction_exponent.json")))

    (define y_number_real_neg_exp.json
      (check #(0.01) (parse "./files/y_number_real_neg_exp.json")))

    (define y_number_real_pos_exponent.json
      (check #(100.0) (parse "./files/y_number_real_pos_exponent.json")))

    (define y_number_simple_int.json
      (check #(123) (parse "./files/y_number_simple_int.json")))

    (define y_number_simple_real.json
      (check #(123.456789) (parse "./files/y_number_simple_real.json")))

    (define y_object_basic.json
      (check '((asd . "sdf")) (parse "./files/y_object_basic.json")))

    (define y_object_duplicated_key_and_value.json
      (check '((a . "b") (a . "b")) (parse "./files/y_object_duplicated_key_and_value.json")))

    (define y_object_duplicated_key.json
      (check '((a . "b") (a . "c")) (parse "./files/y_object_duplicated_key.json")))

    (define y_object_empty.json
      (check '() (parse "./files/y_object_empty.json")))

    (define y_object_empty_key.json
      (check '((|| . 0)) (parse "./files/y_object_empty_key.json")))

    ;; TODO: add escaped null char
    (define y_object_escaped_null_in_key.json
      (skip check '((|foobar| . 42)) (parse "./files/y_object_escaped_null_in_key.json")))

    (define y_object_extreme_numbers.json
      (check '((min . -1e+28) (max . 1e+28))
             (parse "./files/y_object_extreme_numbers.json")))

    (define y_object.json
      (check '((asd . "sdf") (dfg . "fgh")) (parse "./files/y_object.json")))

    (define y_object_long_strings.json
      (check '((abc . #(((def . "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))))
               (ijk . "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"))
             (parse "./files/y_object_long_strings.json")))

    (define y_object_simple.json
      (check '((a . #())) (parse "./files/y_object_simple.json")))

    (define y_object_string_unicode.json
      (check '((title . "Полтора Землекопа")) (parse "./files/y_object_string_unicode.json")))

    (define y_object_with_newlines.json
      (check '((a . "b")) (parse "./files/y_object_with_newlines.json")))

    (define y_string_1_2_3_bytes_UTF-8_sequences.json
      (check #("`Īካ") (parse "./files/y_string_1_2_3_bytes_UTF-8_sequences.json")))

    ;; XXX: dubious
    (define y_string_accepted_surrogate_pair.json
      (skip check #("������") (parse "./files/y_string_accepted_surrogate_pair.json")))

    ;; XXX: dubious result check
    (define y_string_accepted_surrogate_pairs.json
      (skip check #("������������") (parse "./files/y_string_accepted_surrogate_pairs.json")))

    (define y_string_allowed_escapes.json
      (check #("\"\\/\b\x0c;\n\r\t") (parse "./files/y_string_allowed_escapes.json")))

    (define y_string_backslash_and_u_escaped_zero.json
      (check #("\\u0000") (parse "./files/y_string_backslash_and_u_escaped_zero.json")))

    (define y_string_backslash_doublequotes.json
      (check #("\"") (parse "./files/y_string_backslash_doublequotes.json")))

    (define y_string_comments.json
      (check #("a/*b*/c/*d//e") (parse "./files/y_string_comments.json")))

    (define y_string_double_escape_a.json
      (check #("\\a") (parse "./files/y_string_double_escape_a.json")))

    (define y_string_double_escape_n.json
      (check #("\\n") (parse "./files/y_string_double_escape_n.json")))

    (define y_string_escaped_control_character.json
      (check #("\x12;") (parse "./files/y_string_escaped_control_character.json")))

    (define y_string_escaped_noncharacter.json
      (check #("￿") (parse "./files/y_string_escaped_noncharacter.json")))

    (define y_string_in_array.json
      (check #("asd") (parse "./files/y_string_in_array.json")))

    (define y_string_in_array_with_leading_space.json
      (check #("asd") (parse "./files/y_string_in_array_with_leading_space.json")))

    ;; XXX: result is suspect
    (define y_string_last_surrogates_1_and_2.json
      (skip check #("������") (parse "./files/y_string_last_surrogates_1_and_2.json")))

    (define y_string_nbsp_uescaped.json
      (check #("new line") (parse "./files/y_string_nbsp_uescaped.json")))

    (define y_string_nonCharacterInUTF-8_U+10FFFF.json
      (check #("􏿿") (parse "./files/y_string_nonCharacterInUTF-8_U+10FFFF.json")))

    (define y_string_nonCharacterInUTF-8_U+FFFF.json
      (check #("￿") (parse "./files/y_string_nonCharacterInUTF-8_U+FFFF.json")))

    (define y_string_null_escape.json
      (check #("\x00;") (parse "./files/y_string_null_escape.json")))

    (define y_string_one-byte-utf-8.json
      (check #(",") (parse "./files/y_string_one-byte-utf-8.json")))

    (define y_string_pi.json
      (check #("π") (parse "./files/y_string_pi.json")))

    (define y_string_reservedCharacterInUTF-8_U+1BFFF.json
      (check #("𛿿") (parse "./files/y_string_reservedCharacterInUTF-8_U+1BFFF.json")))

    (define y_string_simple_ascii.json
      (check #("asd ") (parse "./files/y_string_simple_ascii.json")))

    (define y_string_space.json
      (check " " (parse "./files/y_string_space.json")))

    ;; XXX: result is suspect
    (define y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json
      (skip check #("������") (parse "./files/y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF.json")))

    (define y_string_three-byte-utf-8.json
      (check #("ࠡ") (parse "./files/y_string_three-byte-utf-8.json")))

    (define y_string_two-byte-utf-8.json
      (check #("ģ") (parse "./files/y_string_two-byte-utf-8.json")))

    (define y_string_u+2028_line_sep.json
      (check #(" ") (parse "./files/y_string_u+2028_line_sep.json")))

    (define y_string_u+2029_par_sep.json
      (check #(" ") (parse "./files/y_string_u+2029_par_sep.json")))

    (define y_string_uescaped_newline.json
      (check #("new\nline") (parse "./files/y_string_uescaped_newline.json")))

    (define y_string_uEscape.json
      (check #("aクリス") (parse "./files/y_string_uEscape.json")))

    ;; XXX: copy pasting from the terminal does not work
    (define y_string_unescaped_char_delete.json
      (skip check #("") (parse "./files/y_string_unescaped_char_delete.json")))

    (define y_string_unicode_2.json
      (check #("⍂㈴⍂") (parse "./files/y_string_unicode_2.json")))

    (define y_string_unicodeEscapedBackslash.json
      (check #("\\") (parse "./files/y_string_unicodeEscapedBackslash.json")))

    (define y_string_unicode_escaped_double_quote.json
      (check #("\"") (parse "./files/y_string_unicode_escaped_double_quote.json")))

    (define y_string_unicode.json
      (check #("ꙭ") (parse "./files/y_string_unicode.json")))

    ;; XXX: expected value is dubious
    (define y_string_unicode_U+10FFFE_nonchar.json
      (skip check #("������") (parse "./files/y_string_unicode_U+10FFFE_nonchar.json")))

    ;; XXX: expected value is dubious
    (define y_string_unicode_U+1FFFE_nonchar.json
      (skip check #("������") (parse "./files/y_string_unicode_U+1FFFE_nonchar.json")))

    (define y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json
      (check #("​") (parse "./files/y_string_unicode_U+200B_ZERO_WIDTH_SPACE.json")))

    (define y_string_unicode_U+2064_invisible_plus.json
      (check #("⁤") (parse "./files/y_string_unicode_U+2064_invisible_plus.json")))

    (define y_string_unicode_U+FDD0_nonchar.json
      (check #("﷐") (parse "./files/y_string_unicode_U+FDD0_nonchar.json")))

    ;; XXX: This is different fron CPython 3.6
    (define y_string_unicode_U+FFFE_nonchar.json
      (check #("￾") (parse "./files/y_string_unicode_U+FFFE_nonchar.json")))

    (define y_string_utf8.json
      (check #("€𝄞") (parse "./files/y_string_utf8.json")))

    (define y_string_with_del_character.json
      (check #("aa") (parse "./files/y_string_with_del_character.json")))

    (define y_structure_lonely_false.json
      (check #f (parse "./files/y_structure_lonely_false.json")))

    (define y_structure_lonely_int.json
      (check 42 (parse "./files/y_structure_lonely_int.json")))

    (define y_structure_lonely_negative_real.json
      (check -0.1 (parse "./files/y_structure_lonely_negative_real.json")))

    (define y_structure_lonely_null.json
      (check 'null (parse "./files/y_structure_lonely_null.json")))

    (define y_structure_lonely_string.json
      (check "asd" (parse "./files/y_structure_lonely_string.json")))

    (define y_structure_lonely_true.json
      (check #t (parse "./files/y_structure_lonely_true.json")))

    (define y_structure_string_empty.json
      (check "" (parse "./files/y_structure_string_empty.json")))

    (define y_structure_trailing_newline.json
      (check #("a") (parse "./files/y_structure_trailing_newline.json")))

    (define y_structure_true_in_array.json
      (check #(#t) (parse "./files/y_structure_true_in_array.json")))

    (define y_structure_whitespace_array.json
      (check #() (parse "./files/y_structure_whitespace_array.json")))

    ;; Other tests

    (define y_object_nested.json
      (check '((outer (inner . 1))) (parse "./files/y_object_nested.json")))

    ;; Scheme specific tests

    (define n_+inf.0
      (check-raise json-error? (obj->json-string +inf.0)))

    (define n_-inf.0
      (check-raise json-error? (obj->json-string -inf.0)))

    (define n_complex
      (check-raise json-error? (obj->json-string 3+14i)))

    (define n_-nan.0
      (check-raise json-error? (obj->json-string +nan.0)))

    (define n_+nan.0
      (check-raise json-error? (obj->json-string -nan.0)))

    (define n_exact_not_integer
      (check-raise json-error? (obj->json-string 314/100)))

    (define y_json_lines_numbers
      (check '(1 2 3) (call-with-input-string "1\n2\n3\n"
                        (lambda (port)
                          (let loop ((obj (json-read port))
                                     (out '()))
                            (if (eof-object? obj)
                                (reverse out)
                                (loop (json-read port) (cons obj out))))))))

    (define y_json_lines_arrays
      (check '(#(1) #(2) #(3))
             (call-with-input-string "[1]\n[2]\n[3]\n"
                                     (lambda (port)
                                       (let loop ((obj (json-read port))
                                                  (out '()))
                                         (if (eof-object? obj)
                                             (reverse out)
                                             (loop (json-read port) (cons obj out))))))))

    (define y_json_lines_objects
      (check '(((hello . "world")) ((true . #t)) ((magic . 42)))
             (call-with-input-string "{\"hello\": \"world\"}\n{\"true\": true}\n{\"magic\": 42}"
                                     (lambda (port)
                                       (let loop ((obj (json-read port))
                                                  (out '()))
                                         (if (eof-object? obj)
                                             (reverse out)
                                             (loop (json-read port) (cons obj out))))))))

    (define character-limit
      (check-raise json-error?
                   (parameterize ((json-number-of-character-limit 1))
                     (json-string->obj "3.14159"))))

    (define nesting-limit
      (check-raise json-error?
                   (parameterize ((json-nesting-depth-limit 1))
                     (json-string->obj "[[3.14159]]"))))

    ;; parse json into records

    (define-record-type <magic>
      (make-magic number)
      magic?
      (number magic-number))

    (define (json-magic port)
      (define %root '(root))

      (define (array-start seed) '())

      (define (array-end items)
        (list->vector (reverse items)))

      (define (object-start seed) '())

      (define (plist->record plist)
        (make-magic (car plist)))

      (define object-end plist->record)

      (define (proc obj seed)
        (if (eq? seed %root)
            obj
            (cons obj seed)))

      (let ((out (json-fold proc
                            array-start
                            array-end
                            object-start
                            object-end
                            %root
                            port)))
        ;; if out is the root object, then the port or generator is empty.
        (if (eq? out %root)
            (eof-object)
            out)))

    (define parse-into-records
      (check #(42 101 1337 2006)
             (vector-map magic-number (call-with-input-string "[
{\"magic\": 42},
{\"magic\": 101},
{\"magic\": 1337},
{\"magic\": 2006}
]" json-magic))))

    (define y_foundationdb_status.scm
      (call-with-input-file "./files/y_foundationdb_status.scm" read))

    (define y_foundationdb_status.json
      (check y_foundationdb_status.scm (parse "./files/y_foundationdb_status.json")))

    ;; sample .jsonl extracted from python-jsonlines that is Copyright
    ;; © 2016, Wouter Bolsterlee, 3-clause "New BSD License" see:
    ;;
    ;;   https://github.com/wbolster/jsonlines/
    ;;
    (define sample-crlf-line-separators.jsonl
      (check '(((a . 1)) ((b . 2)))
             (call-with-input-file "./files/sample-crlf-line-separators.jsonl"
               (lambda (port) (generator->list (json-lines-read port))))))

    (define sample.jsonl
      (check '(((a . 1)) ((b . 2)))
             (call-with-input-file "./files/sample.jsonl"
               (lambda (port) (generator->list (json-lines-read port))))))

    (define sample-no-eol-at-eof.jsonl
      (check '(((a . 1)) ((b . 2)))
             (call-with-input-file "./files/sample-no-eol-at-eof.jsonl"
               (lambda (port) (generator->list (json-lines-read port))))))

    ;; json-sequence.log was taken from:
    ;;
    ;;  https://raw.githubusercontent.com/hildjj/json-text-sequence/
    ;;
    ;; License is MIT:  Copyright (c) 2014 Joe Hildebrand
    ;;
    (define json-sequence.log
      (check '(((d . "2014-09-22T22:11:26.315Z") (count . 0))
               ((d . "2014-09-22T22:11:26.317Z") (count . 1))
               ((d . "2014-09-22T22:11:26.317Z") (count . 2))
               ((d . "2014-09-22T22:11:26.317Z") (count . 3))
               ((d . "2014-09-22T22:11:26.317Z") (count . 4))
               ((d . "2014-09-22T22:11:26.317Z") (count . 5))
               ((d . "2014-09-22T22:11:26.317Z") (count . 6))
               ((d . "2014-09-22T22:11:26.317Z") (count . 7))
               ((d . "2014-09-22T22:11:26.317Z") (count . 8))
               ((d . "2014-09-22T22:11:26.317Z") (count . 9)))
             (call-with-input-file "./files/json-sequence.log"
               (lambda (port) (generator->list (json-sequence-read port))))))

    (define json-sequence-with-one-broken-json.log
      (check '(((d . "2014-09-22T22:11:26.315Z") (count . 0))
               ((d . "2014-09-22T22:11:26.317Z") (count . 1))
               ((d . "2014-09-22T22:11:26.317Z") (count . 2))
               ((d . "2014-09-22T22:11:26.317Z") (count . 3))
               ((d . "2014-09-22T22:11:26.317Z") (count . 4))
               ((d . "2014-09-22T22:11:26.317Z") (count . 5))
               ((d . "2014-09-22T22:11:26.317Z") (count . 6))
               ((d . "2014-09-22T22:11:26.317Z") (count . 7))
               ;; ((d . "2014-09-22T22:11:26.317Z") (count . 8))
               ((d . "2014-09-22T22:11:26.317Z") (count . 9)))
             (call-with-input-file "./files/json-sequence-with-one-broken-json.log"
               (lambda (port) (generator->list (json-sequence-read port))))))

    (define json-generator-single-top-level-value
      (check
        (call-with-input-string "42 101 1337" (lambda (port) (generator->list (json-generator port))))
        '(42)))

    (define json-generator-single-top-level-value-structure
      (check
        (call-with-input-string "[42] 101 1337" (lambda (port) (generator->list (json-generator port))))
        '(array-start 42 array-end)))))
