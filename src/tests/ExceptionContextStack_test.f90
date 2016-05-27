program ExceptionContextStack_test

USE ExceptionContextStack

implicit none

    type(ExceptionContextStack_t)         :: TheExceptionContextStack

    call TheExceptionContextStack%Push(__FILE__, __LINE__)
    call TheExceptionContextStack%Push(__FILE__, __LINE__)
    call TheExceptionContextStack%Push(__FILE__, __LINE__)
    call TheExceptionContextStack%Push(__FILE__, __LINE__)
    call TheExceptionContextStack%Push(__FILE__, __LINE__)
    call TheExceptionContextStack%Push(__FILE__, __LINE__)
    if(.not. TheExceptionContextStack%isEmpty()) call TheExceptionContextStack%Print()
    call TheExceptionContextStack%Free()
    if(TheExceptionContextStack%isEmpty()) print*, 'Empty after call Free()'


end program ExceptionContextStack_test
