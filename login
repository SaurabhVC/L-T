
*&---------------------------------------------------------------------*
*   Program Name         : ZCRM_FUNC_LOGIN_PROCESS
*   Category             : New
*   Purpose              : User authentication
*   Details              : This fm will take username and password as
*                          input and will give login related error and
*                          user related error as output. if user login
*                          is successful person id related to that
*                          username will be output
*   Development Class    : ZPK_KDMOBILITY
*   Author               : Saurabh Chikate
*   Start Date           :
*   End Date             : 24-Feb-2016
*   Business Analyst     :
*   Ticket number        :
*   WIMS   number        :
*   Trans. Req. No.      : CRDK908499
*&--------------------------------------------------------------------*



FUNCTION zcrm_func_login_process.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_USERNAME) TYPE  RSYST-BNAME
*"     VALUE(IV_PASSWORD) TYPE  RSYST-BCODE
*"  EXPORTING
*"     VALUE(EV_PERSON_ID) TYPE  PERSONID
*"     VALUE(EV_LOGIN_MESSAGE) TYPE  CHAR100
*"     VALUE(EV_USER_MESSAGE) TYPE  CHAR100
*"     VALUE(EV_USER_FLAG) TYPE  CHAR1
*"     VALUE(EV_LOGIN_FLAG) TYPE  CHAR1
*"     VALUE(EV_QUERY_MESSAGE) TYPE  CHAR100
*"     VALUE(EV_FNAME) TYPE  BU_MCNAME2
*"     VALUE(EV_LNAME) TYPE  BU_MCNAME1
*"----------------------------------------------------------------------
*DATA : lv_exception TYPE char200.


  DATA : lv_person_id TYPE char10.
  DATA : hc_class TYPE REF TO zcl_kd_global_mobility.
  DATA : flag_star TYPE char1 VALUE '0'.
  DATA : flag_star_office TYPE char1 VALUE '0'.
  CREATE OBJECT hc_class.

  DATA : descr TYPE REF TO cl_crm_ppm_um_toolkit.
  CREATE OBJECT descr.

  DATA : wa_ot_tab TYPE zcrm_str_user_login.

  DATA : import TYPE OBJEKTID.

  TYPES: BEGIN OF ls_agr_users,
    agr_name  TYPE agr_name,
        END OF ls_agr_users.

  TYPES:
  BEGIN OF ls_sales,
    agr_name  TYPE agr_name,
    field     TYPE agrfield,
    low       TYPE agval,
  END OF ls_sales.

  TYPES:
    BEGIN OF lt_slorgs,
      low       TYPE crmt_sales_org,
    END OF lt_slorgs.

  TYPES:
    begin of lt_sloffice,
      office TYPE  CRMT_SALES_OFFICE,
    END OF lt_sloffice.

  DATA : lt_agr_users TYPE TABLE OF ls_agr_users.
  DATA : lt_sales_org TYPE TABLE OF ls_sales    WITH HEADER LINE.
  DATA : lt_slorgs    TYPE TABLE OF lt_slorgs      WITH HEADER LINE .
  DATA : lt_sloffices TYPE TABLE OF lt_sloffice WITH HEADER LINE.

  ev_user_flag = hc_class->fail_flag.
  ev_login_flag = hc_class->fail_flag.
  ev_login_message = hc_class->no_person.

*► translate username to uppercase ◄
  TRANSLATE iv_username TO UPPER CASE.

  CALL FUNCTION 'BP_CENTRALPERSON_GET'
    EXPORTING
*     IV_PERSON_ID         =
*     IV_BU_PARTNER_GUID   =
*     IV_EMPLOYEE_ID       =
      iv_username          = iv_username
    IMPORTING
      ev_person_id         = ev_person_id
*     EV_BU_PARTNER_GUID   =
*     EV_USERNAME          =
*     ET_EMPLOYEE_ID       =
*     EV_NAME              =
*     ET_USERS             =
*     ET_EMPLOYEE_ID_STAT2 =
    EXCEPTIONS
      no_central_person    = 1
      no_business_partner  = 2
      no_id                = 3
      OTHERS               = 4.

  ev_user_message = 'Login Failed'.
  ev_user_flag = 'F'.

  IF sy-subrc EQ 0.
    ev_user_message = hc_class->user_found.
    ev_user_flag = hc_class->success_flag.
  ENDIF.

  IF sy-subrc EQ 1.
    ev_user_message = hc_class->no_person.
    ev_user_flag = hc_class->fail_flag.
  ENDIF.

  IF sy-subrc EQ 2.
    ev_user_message = hc_class->no_business_part.
    ev_user_flag = 'F'.
  ENDIF.

  IF sy-subrc EQ 3.
    ev_user_message = hc_class->no_id.
    ev_user_flag = hc_class->fail_flag.
  ENDIF.

  IF sy-subrc GE 4.
    ev_user_message = hc_class->other_error.
    ev_user_flag = hc_class->fail_flag.
  ENDIF.

*► FM to check if user exist or not ◄
  IF sy-subrc EQ 0.

    CALL FUNCTION 'SUSR_LOGIN_CHECK_RFC'
      EXPORTING
        bname                     = iv_username
        password                  = iv_password
*       XBCODE                    =
*       XCODVN                    =
*       USE_NEW_EXCEPTION         = 0
      EXCEPTIONS
        wait                      = 1
        user_locked               = 2
        user_not_active           = 3
        password_expired          = 4
        wrong_password            = 5
        no_check_for_this_user    = 6
        password_attempts_limited = 7
        internal_error            = 8
        OTHERS                    = 9.

CONCATENATE 'EM' Ev_person_id INTO lv_person_id .

    SELECT SINGLE mc_name1 mc_name2
      FROM but000
      into (ev_lname,ev_fname)
      WHERE PARTNER eq lv_person_id .

    IF sy-subrc EQ 0.
      ev_login_message = hc_class->login_success.
      ev_login_flag = hc_class->success_flag.
    ENDIF.

    IF sy-subrc EQ 1.
      ev_login_message = hc_class->wait_message.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 2.
      ev_login_message = hc_class->user_locked.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 3.
      ev_login_message = hc_class->user_not_active.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 4.
      ev_login_message = hc_class->password_expired.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 5.
      ev_login_message = hc_class->wrong_password.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 6.
      ev_login_message = hc_class->no_check.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 7.
      ev_login_message = hc_class->pass_attempt_limited.
      ev_login_flag = hc_class->pass_attempt_limited.
    ENDIF.

    IF sy-subrc EQ 8.
      ev_login_message = hc_class->pass_internal_err.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.

    IF sy-subrc EQ 9.
      ev_login_message = hc_class->other_error.
      ev_login_flag = hc_class->fail_flag.
    ENDIF.


  ENDIF.


*messages when password is not provided in OData
  IF iv_password IS INITIAL .
    ev_user_flag = hc_class->fail_flag .
    ev_login_flag = hc_class->fail_flag.
    ev_login_message = hc_class->wrong_password.
    ev_user_message = hc_class->wrong_password.
  ENDIF.

ENDFUNCTION.
