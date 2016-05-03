module MyCustomExceptions

USE BaseException

implicit none
private

    type, extends(Exception)       :: Exception_t
    end type

    type, extends(Exception)       :: ParentException
    end type

    type, extends(ParentException) :: ChildException
    end type

    type, extends(Exception) :: FatalException
    contains
        procedure, public :: Catch  => FatalException_Catch
    end type

    interface ParentException
        module procedure ParentException_Constructor
    end interface

    interface ChildException
        module procedure ChildException_Constructor
    end interface

    interface FatalException
        module procedure FatalException_Constructor
    end interface

public :: ParentException, ChildException, FatalException

contains
    function ParentException_Constructor() result(aParentException)
    !-----------------------------------------------------------------
    !< Set the default code and message and return a ParentException
    !-----------------------------------------------------------------
        class(Exception), allocatable :: aParentException
    !-----------------------------------------------------------------
        allocate(ParentException::aParentException)
        call aParentException%Create(Code = -1, Message = 'A Parent exception!')
    end function


    function ChildException_Constructor() result(aChildException)
    !-----------------------------------------------------------------
    !< Set the default code and message and return a ChildException
    !-----------------------------------------------------------------
        class(Exception), allocatable :: aChildException
    !-----------------------------------------------------------------
        allocate(ChildException::aChildException)
        call aChildException%Create(Code = -2, Message = 'A Child exception!')
    end function


    function FatalException_Constructor() result(aFatalException)
    !-----------------------------------------------------------------
    !< Set the default code and message and return a FatalException
    !-----------------------------------------------------------------
        class(Exception), allocatable :: aFatalException
    !-----------------------------------------------------------------
        allocate(FatalException::aFatalException)
        call aFatalException%Create(Code = -3, Message = 'A Fatal exception!')
    end function

    subroutine FatalException_Catch(this) 
    !-----------------------------------------------------------------
    !< Automatic accion if an Fatal exception is catched
    !-----------------------------------------------------------------
        class(FatalException), intent(in) :: this
    !-----------------------------------------------------------------
        print*, 'Customized catch action for FatalException'
!        call exit(this%GetCode()) ! Stops program with an error code
    end subroutine
end module MycustomExceptions


module SomeProcedures

USE BaseException
USE MyCustomExceptions
USE ExceptionStack

implicit none

#include "ExceptionHandler.i90"

contains


    subroutine level1()
        TRY
            call level2()
            THROW(ParentException())
        CATCH(ParentException, E)
            print*, '---> CATCH in level1'
            call E%Print(prefix='[ERROR]')
            THROW(E) ! Rethow
        ENDTRY
    end subroutine

    subroutine level2()
        TRY
            call level3()
        CATCH(ChildException, E)
            print*, '---> CATCH in level2'
            call E%Print(prefix='[ERROR]')
        ENDTRY
    end subroutine

    subroutine level3()
        integer, pointer :: a(:)
        TRY
            nullify(a)
            allocate(a(100))
            THROW(FatalException())
            print*, a ! Not printed. A exceptios was thowed before
        FINALLY
            print*, '---> FINALLY level 3'
            if(associated(a)) deallocate(a) ! It is always performed
        ENDTRY
    end subroutine

end module SomeProcedures


program test_exception

USE BaseException
USE MyCustomExceptions
USE ExceptionStack
USE SomeProcedures

implicit none

#include "ExceptionHandler.i90"

    TRY
        print*, '-> Begin main TRY frame'
        call level1()
    CATCH(ChildException, E)
        print*, '---> CATCH in main: ChildException'
        call E%Print(prefix='[ERROR]')
    CATCH(ParentException, E)
        print*, '---> CATCH in main: ParentException'
        call E%Print(prefix='[ERROR]')
        print*, '-> End main TRY frame'
    ENDTRY

contains


end program

