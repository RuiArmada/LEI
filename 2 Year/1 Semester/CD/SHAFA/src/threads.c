#include "../includes/threads.h"

#ifdef THREADS
bool NO_THREAD = false;
#include <unistd.h>

    #ifdef POSX_THREAD
    #include <pthread.h>

    #elif defined(WIN_THREAD)
    #include <windows.h>

    #endif

#else
bool NO_THREAD = true;
#endif

#ifdef THREADS
typedef struct {
    
    #ifdef POSX_THREAD
        pthread_t previous;
        uintptr_t (* process)(void *);
        uintptr_t (* write)(void *, ERROR, ERROR);
    #elif defined(WIN_THREAD)
        HANDLE previous;
        DWORD (* process)(void *);
        DWORD (* write)(void *, ERROR, ERROR);
    #endif

    void *args;
} DATA;
#endif

#ifdef POSX_THREAD
    static pthread_t THREAD = 0;
#elif defined(WIN_THREAD)
    static HANDLE HTHREAD = 0;
#endif



float clock_thread(CLOCK action) {
    #if defined(THREAD) && (_POSX_C_SOURCE >= 199309L)
        static struct timespec start;
        static bool time_fail = true;
        struct timespec end;

        if(action == START)
            return time_fail = clock_gettime(CLOCK_MONOTONIC, &start);

        else {
            if(time_fail || clock_gettime(CLOCK_MONOTONIC, &end) == -1)
                return -1;

        return (end.tv_sec - start.tv_sec) * 1000 + ((double)(end.tv_nsec - start.tv_nsec)) / 1000000.0;
        }

    #else
        static clock_t start = -1;
        clock_t time;

        if(action == START) {
            start = clock();
            return start != -1 ? 0 : -1;
        } else {
            if(start == -1) 
                return -1;
            time = clock();
            if(time == -1)
                return -1;
            return (float) (((double) (time - start)) / CLOCKS_PER_SEC) * 1000;
        }
    #endif
}



#ifdef POSX_THREAD
    static uintptr_t wrapper(DATA* data) {
        pthread_t previous;
        uintptr_t error, previous_error = _SUCCESS;
        
        error = (*(data->process))(data->args);
        previous = data->previous;
        
        if (previous && pthread_join(previous, (void**) &previous_error))
            previous_error = _THREAD_TERMINATION_FAILED;
        
        error = (*(data->write))(data->args, previous_error, error);

        free(data);
        return previous_error ? previous_error : error;
    }

#elif defined(WIN_THREAD)
    static DWORD WINAPI wrapper(LPVOID _data) {
        DATA* data = (DATA*) _data;
        HANDLE previous_hthread;
        DWORD error, previous_error = _SUCCESS;

        error = (*(data->process))(data->args);
        previous_hthread;

        if(previous_hthread) {
            if(WaitForSingleObject(previous_hthread, INFINITE) != WAIT_OBJECT_0 || !GetExitCodeThread(previous_hthread, &previous_error)) 
                previous_error = _THREAD_TERMINATION_FAILED;
            CloseHandle(previous_hthread);  
        }

        error = (*(data->write))(data->args, previous_error, error);

        HeapFree(GetProcessHeap(), 0, data);
        return previous_error ? previous_error : error;

    }

#endif



ERROR thread_create(ERROR (* process)(void *), ERROR (* write)(void *, ERROR, ERROR), void * args) {
    #ifndef _NO_THREAD
        if(NO_THREAD)
    #endif
        {
            ERROR error;

            error = process(args);
            return write(args, _SUCCESS, error);
        }

    #ifdef POSX_THREAD
        DATA* data = malloc(sizeof(DATA));

        if(!data)
            return _NO_MEMORY;

        *data = (DATA) {
            .previous = THREAD,
            .process = (uintptr_t (*)(void *)) process,
            .write = (uintptr_t (*)(void *, ERROR, ERROR)) write,
            .args = args,
        };

        if(pthread_create(&THREAD, NULL, (void *) wrapper, data)) {
            free(data);
            return _THREAD_CREATION_FAILED;
        }
    
    #elif defined(WIN_THREAD)
        HANDLE handle;
        DATA* data = HeapAlloc(GetProcessHeap(), 0, sizeof(DATA));

        if(!data)
            return _NO_MEMORY;
        
        *data = (DATA) {
            .previous = HTHREAD,
            .process = (DWORD (*) (void *)) process,
            .write = (DWORD (*) (void *, ERROR, ERROR)) write,
            .args = args
        };

        handle = CreateThread(
            NULL,               // Security Attributes
            0,                  // Default Stack Size
            wrapper,            // Thread Function Name
            data,               // Argument to Thread Function
            0,                  // Creation Flags
            NULL                // Returns Thread ID
        );

        if(handle)
            HTHREAD = handle;

        else {
            HeapFree(GetProcessHeap(), 0, data);
            return _THREAD_CREATION_FAILED;
        }
    
    #endif
    return _SUCCESS;
}



ERROR thread_wait() {
    #ifndef _NO_THREAD
        if(NO_THREAD)
    #endif
            return _SUCCESS;

    #ifdef POSX_THREAD
        uintptr_t error;
        if(pthread_join(THREAD, (void**) &error))
            return _THREAD_TERMINATION_FAILED;

    #elif defined(WIN_THREADS)
        DWORD error;

        if(WaitForSingleObject(HTHREAD, INFINITE) != WAIT_OBJECT_0 || !GetExitCodeThread(HTHREAD, &error))
            error = _THREAD_TERMINATION_FAILED;

        CloseHandle(HTHREAD);

    #endif
    return error;
}
