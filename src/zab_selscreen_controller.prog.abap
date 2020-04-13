TYPE-POOLS: abap, sscr .

TABLES: sscrfields.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
SELECTION-SCREEN FUNCTION KEY 4.
SELECTION-SCREEN FUNCTION KEY 5.

CLASS lcl_sel_screen DEFINITION FINAL CREATE PUBLIC .
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_kind,
        parameter     TYPE c VALUE 'P',
        select_option TYPE c VALUE 'S',
      END OF gc_kind.

    METHODS show.
    METHODS constructor .
    METHODS set_input
      IMPORTING
        iv_field TYPE string
        iv_state TYPE abap_bool DEFAULT abap_true .

    METHODS set_invisible
      IMPORTING
        iv_field TYPE string
        iv_state TYPE abap_bool DEFAULT abap_true .

    METHODS set_output
      IMPORTING
        iv_field TYPE string
        iv_state TYPE abap_bool DEFAULT abap_true .

    METHODS set_active
      IMPORTING
        iv_field TYPE string
        iv_state TYPE abap_bool DEFAULT abap_true .


    METHODS set_required
      IMPORTING
        iv_field TYPE string
        iv_state TYPE abap_bool DEFAULT abap_true.

    METHODS enable_options
      IMPORTING
        iv_field   TYPE string
        iv_options TYPE string DEFAULT ''.

    METHODS set_text
      IMPORTING
        iv_field TYPE string
        iv_text  TYPE string.

    METHODS exclude_command
      IMPORTING
        iv_command TYPE sy-ucomm.

    CLASS-METHODS set_value
      IMPORTING
        iv_field TYPE string
        iv_value TYPE string.

    METHODS set_btn
      IMPORTING
        iv_id   TYPE i
        iv_text TYPE gui_ictext OPTIONAL
        iv_icon TYPE icon_d OPTIONAL
        iv_info TYPE gui_info OPTIONAL.

    METHODS disable_interval_check
      IMPORTING
        iv_field TYPE string.

    CLASS-METHODS clear_screen_after_back.
  PRIVATE SECTION.
    DATA:
      mt_screen    TYPE STANDARD TABLE OF screen WITH KEY name,
      mt_texts     TYPE STANDARD TABLE OF rsseltexts,
      mv_key_index TYPE i,
      ms_restrict  TYPE sscr_restrict,
      mt_buttons   TYPE STANDARD TABLE OF smp_dyntxt,
      mt_s_option  TYPE TABLE OF rsldbselop,
      mt_excl_st   TYPE TABLE OF rsexfcode.
ENDCLASS.                    "lcl_sel_screen DEFINITION

CLASS lcl_sel_screen IMPLEMENTATION.

  METHOD constructor.
    LOOP AT SCREEN.
      APPEND screen TO me->mt_screen.
    ENDLOOP.

    DO 5 TIMES.
      APPEND VALUE #( ) TO mt_buttons.
    ENDDO.
  ENDMETHOD.                    "constructor

  METHOD set_btn.
    CHECK iv_id BETWEEN 1 AND 5.
    mt_buttons[ iv_id ] = VALUE #( icon_text  = iv_text
                                   icon_id    = iv_icon
                                   quickinfo  = iv_info ).
  ENDMETHOD.

  METHOD set_value.
    DATA:
      lt_dynpfields TYPE dynpread_tabtype.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
        request    = 'A'
      TABLES
        dynpfields = lt_dynpfields
      EXCEPTIONS
        OTHERS     = 0.

    ASSIGN lt_dynpfields[ fieldname = iv_field ] TO FIELD-SYMBOL(<ls_field_to_change>).
    CHECK sy-subrc = 0.

    <ls_field_to_change>-fieldvalue = iv_value.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfields
      EXCEPTIONS
        OTHERS     = 0.
  ENDMETHOD.

  METHOD enable_options.
    DATA:
      ls_option_list TYPE sscr_opt_list.

    CHECK iv_field IS NOT INITIAL AND iv_options IS NOT INITIAL.
    SPLIT iv_options AT space INTO TABLE DATA(lt_options).

    LOOP AT lt_options ASSIGNING FIELD-SYMBOL(<lv_option>).
      CASE <lv_option>.
        WHEN 'BT'.
          ls_option_list-options-bt = abap_true.
        WHEN 'CP'.
          ls_option_list-options-cp = abap_true.
        WHEN 'EQ'.
          ls_option_list-options-eq = abap_true.
        WHEN 'GE'.
          ls_option_list-options-ge = abap_true.
        WHEN 'GT'.
          ls_option_list-options-gt = abap_true.
        WHEN 'LE'.
          ls_option_list-options-le = abap_true.
        WHEN 'LT'.
          ls_option_list-options-lt = abap_true.
        WHEN 'NB'.
          ls_option_list-options-nb = abap_true.
        WHEN 'NE'.
          ls_option_list-options-ne = abap_true.
        WHEN 'NP'.
          ls_option_list-options-np = abap_true.
      ENDCASE.
    ENDLOOP.

    ls_option_list-name = mv_key_index.
    APPEND ls_option_list TO ms_restrict-opt_list_tab.

    APPEND VALUE #( kind = 'S'
                    name = iv_field
                    sg_main = 'I'
                    sg_addy = space
                    op_main = mv_key_index ) TO ms_restrict-ass_tab.

    mv_key_index = mv_key_index + 1.
  ENDMETHOD.                    "enable_options

  METHOD set_input.
    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<ls_screen>) WHERE name CP |*{ iv_field }*|.
      <ls_screen>-input = COND #( WHEN iv_state = abap_true THEN 1 ELSE 0 ).
    ENDLOOP.
  ENDMETHOD.                    "set_input

  METHOD set_active.
    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<ls_screen>) WHERE name CP |*{ iv_field }*|.
      <ls_screen>-active = COND #( WHEN iv_state = abap_true THEN 1 ELSE 0 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD disable_interval_check.
    APPEND VALUE #( name = iv_field ) TO mt_s_option.
  ENDMETHOD.

  METHOD set_invisible.
    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<ls_screen>) WHERE name CP |*{ iv_field }*|.
      <ls_screen>-invisible = COND #( WHEN iv_state = abap_true THEN 1 ELSE 0 ).
    ENDLOOP.
  ENDMETHOD.                    "set_invisible

  METHOD set_output.
    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<ls_screen>) WHERE name CP |*{ iv_field }*|.
      <ls_screen>-output = COND #( WHEN iv_state = abap_true THEN 1 ELSE 0 ).
    ENDLOOP.
  ENDMETHOD.                    "set_output

  METHOD exclude_command.
    APPEND iv_command TO mt_excl_st.
  ENDMETHOD.

  METHOD set_text.
    FIELD-SYMBOLS:
      <lt_screen_tab> TYPE ANY TABLE.

    ASSIGN ('%_SSCR[]') TO <lt_screen_tab>.
    DO 1 TIMES.
      CHECK <lt_screen_tab> IS ASSIGNED.
      READ TABLE <lt_screen_tab> ASSIGNING FIELD-SYMBOL(<ls_screen_field>) WITH KEY ('NAME') = iv_field.

      CHECK <ls_screen_field> IS ASSIGNED.
      ASSIGN COMPONENT 'KIND' OF STRUCTURE <ls_screen_field> TO FIELD-SYMBOL(<lv_kind>).
      CHECK <lv_kind> IS ASSIGNED.

      APPEND VALUE #( name = iv_field text = iv_text kind = <lv_kind> ) TO mt_texts.
    ENDDO.
  ENDMETHOD.                    "set_text

  METHOD set_required.
    LOOP AT mt_screen ASSIGNING FIELD-SYMBOL(<ls_screen>) WHERE name CP |*{ iv_field }*|.
      <ls_screen>-required = COND #( WHEN iv_state = abap_true THEN 1 ELSE 0 ).
    ENDLOOP.
  ENDMETHOD.                    "set_required

  METHOD show.
    LOOP AT SCREEN.
      READ TABLE mt_screen WITH TABLE KEY name = screen-name ASSIGNING FIELD-SYMBOL(<ls_screen>).
      IF sy-subrc IS INITIAL.
        MODIFY SCREEN FROM <ls_screen>.
      ENDIF.
    ENDLOOP.

    " Настройка ограничений Select options
    CALL FUNCTION 'RS_INT_SELOPT_RESTRICT'
      EXPORTING
        program     = sy-cprog
        restriction = ms_restrict
      EXCEPTIONS
        OTHERS      = 99.

    CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
      EXPORTING
        program  = sy-cprog
      TABLES
        seltexts = mt_texts
      EXCEPTIONS
        OTHERS   = 99.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
      TABLES
        p_exclude = mt_excl_st.

    sscrfields-functxt_01 = mt_buttons[ 1 ].
    sscrfields-functxt_02 = mt_buttons[ 2 ].
    sscrfields-functxt_03 = mt_buttons[ 3 ].
    sscrfields-functxt_04 = mt_buttons[ 4 ].
    sscrfields-functxt_05 = mt_buttons[ 5 ].

    CALL FUNCTION 'RS_SELOPT_NO_INTERVAL_CHECK'
      EXPORTING
        program        = sy-repid
      TABLES
        selop          = mt_s_option
      EXCEPTIONS
        no_programname = 1
        OTHERS         = 2.

  ENDMETHOD.

  METHOD clear_screen_after_back.
    FIELD-SYMBOLS : <fs> TYPE any.
    ASSIGN ('(RSDBRUNT)MEMKEY-INT_MODE') TO <fs> .
    IF sy-subrc = 0 .
      <fs> = '01' .
    ENDIF.
  ENDMETHOD.

ENDCLASS.
