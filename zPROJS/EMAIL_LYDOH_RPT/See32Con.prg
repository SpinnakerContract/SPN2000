***  SEE32CON.FOX
***
***  DEFINE's for Visual FoxPro 3.0 and above
***
***  For SMTP/POP3 Email Engine (SEE) Version 3.7
***

#define SEE_MIN_RESPONSE_WAIT    1
#define SEE_MAX_RESPONSE_WAIT    2
#define SEE_CONNECT_WAIT         3
#define SEE_DISABLE_MIME         4
#define SEE_QUOTED_PRINTABLE     8
#define SEE_AUTO_CALL_DRIVER     9
#define SEE_FILE_PREFIX         10
#define SEE_SLEEP_TIME          13
#define SEE_DECODE_UNNAMED      14
#define SEE_SMTP_PORT           15
#define SEE_POP3_PORT           16
#define SEE_MAX_LINE_LENGTH     17
#define SEE_BLOCKING_MODE       18
#define SEE_ALLOW_8BITS         19
#define SEE_LOG_FILE            20
#define SEE_HIDE_SAVED_MSG      21
#define SEE_HIDE_TO_ADDR        22
#define SEE_ADDRESS_DELIMITER   23
#define SEE_WSACLEANUP          24
#define SEE_PATH_DELIMITER      25
#define SEE_ATTACH_DELIMITER    26
#define SEE_ENABLE_IMAGE        27
#define SEE_RAW_MODE            28
#define SEE_ENABLE_ESMTP        29
#define SEE_ENABLE_APOP         30
#define SEE_ATTACH_BASE_NUMBER  31
#define SEE_IGNORE_REJECTED     32
#define SEE_WRITE_CONTENT_TYPE  33
#define SEE_SET_FILE_PREFIX     34
#define SEE_HTML_CHARSET        35
#define SEE_HIDE_HEADERS        36

#define CHARSET_BLANK        0
#define CHARSET_US_ASCII     1
#define CHARSET_8859         4
#define CHARSET_ISO_8859_1   4
#define CHARSET_ISO_8859_8   5
#define CHARSET_WIN_1252     6
#define CHARSET_WIN_1255     7

#define SEE_GET_ERROR_TEXT       1
#define SEE_GET_COUNTER          2
#define SEE_GET_RESPONSE         3
#define SEE_GET_SOCK_ERROR       4

#define SEE_GET_MESSAGE_BYTES_READ  10
#define SEE_GET_ATTACH_BYTES_READ   11
#define SEE_GET_TOTAL_BYTES_READ    12
#define SEE_GET_MESSAGE_BYTES_SENT  13
#define SEE_GET_ATTACH_BYTES_SENT   14
#define SEE_GET_TOTAL_BYTES_SENT    15
#define SEE_GET_VERSION             16
#define SEE_GET_MSG_COUNT           17
#define SEE_GET_MSG_SIZE            18
#define SEE_GET_BUFFER_COUNT        19
#define SEE_GET_CONNECT_STATUS      20
#define SEE_GET_REGISTRATION        21
#define SEE_GET_ATTACH_COUNT        22
#define SEE_GET_LAST_RESPONSE       23
#define SEE_GET_VERIFY_STATUS       24
#define SEE_GET_SERVER_IP           25
#define SEE_GET_BUILD               26
#define SEE_GET_SOCKET              27
#define SEE_GET_LOCAL_IP            28
#define SEE_GET_ATTACH_NAMES        29
#define SEE_GET_LAST_RECIPIENT      30

#define SEE_COPY_BUFFER             40
#define SEE_WRITE_BUFFER            41

#define SEE_SET_REPLY               50
#define SEE_SET_HEADER              51
#define SEE_WRITE_TO_LOG            52
#define SEE_SET_FROM                53
#define SEE_SET_CONTENT_TYPE        54
#define SEE_SET_TRANSFER_ENCODING   55
#define SEE_ADD_HEADER              56
#define SEE_SET_SECRET              57
#define SEE_SET_USER                58
#define SEE_SET_TEXT_MESSAGE        59
#define SEE_FORCE_INLINE            60
#define SEE_SET_ATTACH_CONTENT_TYPE 61
#define SEE_AUTHENTICATE_PROTOCOL   62
#define SEE_SET_CONTENT_TYPE_PREFIX 63
#define SEE_RESERVED_XXX_1          64
#define SEE_SET_DEFAULT_ZONE        65
#define SEE_SET_ATTACH_DESCRIPTION  66
#define SEE_REPLACE_WITH_COMMAS     67

#define QUOTED_OFF        0
#define QUOTED_PLAIN      1
#define QUOTED_HTML       2
#define QUOTED_RICH       3
#define QUOTED_8859       4
#define QUOTED_ISO_8859_1 4
#define QUOTED_ISO_8859_8 5
#define QUOTED_WIN_1252   6
#define QUOTED_WIN_1255   7
#define QUOTED_USER       9

#define INLINE_TEXT_OFF         0
#define INLINE_TEXT_INLINE      1
#define INLINE_TEXT_ATTACHMENT  2

#define AUTHENTICATE_CRAM    1
#define AUTHENTICATE_LOGIN   2
#define AUTHENTICATE_PLAIN   4
