program ExceptionStackNode_test

USE BaseException
USE ExceptionStackNode

implicit none

    type(ExceptionStackNode_t) :: StackNode

    call StackNode%SetException(anException=Exception(Code=999, Message='This is a generic exception'), File=__FILE__, Line=__LINE__)
    call StackNode%Print()

end program ExceptionStackNode_test
