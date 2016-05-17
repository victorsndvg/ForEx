program ExceptionContextStackNode_test

USE ExceptionContextStackNode

implicit none

    type(ExceptionContextStackNode_t) :: ContextStackNode

    call ContextStackNode%SetContext( File=__FILE__, Line=__LINE__)
    call ContextStackNode%Print(prefix='[Backtrace]')
    call ContextStackNode%Free()

end program ExceptionContextStackNode_test
