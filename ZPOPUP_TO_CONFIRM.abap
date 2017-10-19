  DATA: ld_popup_return   TYPE string .

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmación'
      text_question         = 'Desea realizar formula determinación volumen'
      text_button_1         = 'Si'
      text_button_2         = 'No'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = ld_popup_return " to hold the FM's return value
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK ld_popup_return = 1 .