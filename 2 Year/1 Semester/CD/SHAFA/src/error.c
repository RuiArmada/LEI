#include "../includes/error.h"

#define FAILED(_)                                                                              \
    _(_SUCCESS, "No ERROR Found.\n"                                                      )     \
    _(_NO_MEMORY, "Not Enough Memory (ERROR)\n"                                          )     \
    _(_FILE_INACCESSIBLE, "File can't be accessed (ERROR)\n"                             )     \
    _(_FILE_UNRECOGNIZABLE, "File not recognized (ERROR)\n"                              )     \
    _(_FILE_STREAM_FAILED, "Can't communicate properly with file's stream (ERROR)\n"     )     \
    _(_FILE_TOO_SMALL, "File too small for decompression (ERROR)\n"                      )     \
    _(_THREAD_CREATION_FAILED, "Thread couldn't be created (ERROR)\n"                    )     \
    _(_THREAD_TERMINATION_FAILED, "Thread didn't terminate properly (ERROR)\n"           )     
    
#define ERROR_CASE(NUM, MSG) case NUM: return MSG;

char* hades_prtcl(int num) {
    switch(num) {
        FAILED(ERROR_CASE)
    }
    return "Unknown ERROR";
}
