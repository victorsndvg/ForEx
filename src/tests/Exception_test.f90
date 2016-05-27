module MyCustomException

USE BaseException

implicit none
private
    type, extends(Exception) :: CustomException
    end type

    interface CustomException
        module procedure CustomException_Constructor
    end interface

public :: CustomException

contains
    function CustomException_Constructor() result(aCustomException)
    !-----------------------------------------------------------------
    !< Set the default code and message and return a CustomException
    !-----------------------------------------------------------------
        class(Exception), allocatable :: aCustomException
    !-----------------------------------------------------------------
        allocate(CustomException::aCustomException)
        call aCustomException%Create(Code = -1, Message = 'My custom Exception!')
    end function
end module MycustomException


program Exception_test

USE BaseException
USE MyCustomException

implicit none


    class(Exception), allocatable :: theException

    allocate(Exception::theException)

    call TheException%Create(Code=999, Message='This is a generic exception')
    call TheException%AddContext(File=__FILE__, Line=__LINE__)
    call TheException%AddContext(File=__FILE__, Line=__LINE__)
    call TheException%Print(prefix='[Error]')

    select type(TheException)
        class is (Exception)
            print*, '-> class is (Exception)'
        class is (CustomException)
            print*, '-> class is (CustomException)'
    end select

    call theException%Free()
    deallocate(theException)

    allocate(CustomException::theException)

    call theException%Create(Code=999, Message='This is a generic exception')
    call TheException%AddContext(File=__FILE__, Line=__LINE__)
    call TheException%Print(prefix='[Error]')

    select type(TheException)
        class is (Exception)
            print*, '-> class is (Exception)'
        class is (CustomException)
            print*, '-> class is (CustomException)'
    end select

    call theException%Free()
    deallocate(theException)

end program Exception_test
