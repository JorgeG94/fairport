! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Stable numeric identifiers for every event kind.
!!
!! GENERATED FILE -- DO NOT EDIT.
!! Produced by `tools/autogen/core_event_kinds.fypp`; edit the template and
!! rerun `tools/autogen/autogen.sh` instead.
module core_event_kinds
   !! One append-only list generates the constants, the name lookup and the
   !! bus dispatch bound, so the three can never drift apart.
   use core_kinds, only: int16
   implicit none
   private

   public :: event_name
   public :: MAX_EVENT_KIND
   public :: N_EVENT_KINDS

   integer(int16), parameter, public :: K_TOUCHDOWN = 1_int16
      !! An arrival has touched down.
   integer(int16), parameter, public :: K_ROLLOUTCOMPLETE = 2_int16
      !! The landing roll is over.
   integer(int16), parameter, public :: K_RUNWAYEXITED = 3_int16
      !! The runway is clear behind an arrival.
   integer(int16), parameter, public :: K_TAXINODEREACHED = 4_int16
      !! An aircraft reached a taxiway node.
   integer(int16), parameter, public :: K_ONBLOCKS = 5_int16
      !! An aircraft is parked on a gate.
   integer(int16), parameter, public :: K_TURNAROUNDCOMPLETE = 6_int16
      !! A turnaround has finished.
   integer(int16), parameter, public :: K_PUSHBACKREQUESTED = 7_int16
      !! A departure has requested pushback.
   integer(int16), parameter, public :: K_PUSHBACKCOMPLETE = 8_int16
      !! Pushback is complete.
   integer(int16), parameter, public :: K_LINEUPCLEARED = 9_int16
      !! A departure is cleared to line up.
   integer(int16), parameter, public :: K_TAKEOFFROLLCOMPLETE = 10_int16
      !! A departure has left the ground.
   integer(int16), parameter, public :: K_BINGOFUEL = 11_int16
      !! A holding arrival reached bingo fuel.
   integer(int16), parameter, public :: K_HOLDENTERED = 12_int16
      !! An arrival entered the holding stack.
   integer(int16), parameter, public :: K_CAPACITYCHANGED = 13_int16
      !! Declared capacity has changed.
   integer(int16), parameter, public :: K_WEATHERCHANGED = 14_int16
      !! The weather has changed.
   integer(int16), parameter, public :: K_GATEASSIGNED = 15_int16
      !! A gate has been assigned.
   integer(int16), parameter, public :: K_GATERELEASED = 16_int16
      !! A gate has been given up.
   integer(int16), parameter, public :: K_REPLANREQUESTED = 17_int16
      !! A route must be recomputed.
   integer(int16), parameter, public :: K_INCIDENTTRIGGERED = 18_int16
      !! An incident has fired.
   integer(int16), parameter, public :: K_LINEUPREQUESTED = 19_int16
      !! A departure is at the holding point.
   integer(int16), parameter, public :: K_PASSENGERENTEREDCONCOURSE = 40_int16
      !! Landside, milestone 3.
   integer(int16), parameter, public :: K_PASSENGEREXITEDARRIVALS = 41_int16
      !! Landside, milestone 3.
   integer(int16), parameter, public :: K_PURCHASEMADE = 42_int16
      !! Landside, milestone 3.
   integer(int16), parameter, public :: K_CURBQUEUECHANGED = 43_int16
      !! Landside, milestone 3.
   integer(int16), parameter, public :: K_BAGLOADED = 44_int16
      !! Landside, milestone 3.
   integer(int16), parameter, public :: K_BAGSTOLEN = 45_int16
      !! Landside, milestone 3.

   integer(int16), parameter, public :: K_CMD_ASSIGN_GATE = 60_int16
      !! Player command: Park an arrival on a named stand.
   integer(int16), parameter, public :: K_CMD_HOLD_DEPARTURE = 61_int16
      !! Player command: Keep a departure on its stand.
   integer(int16), parameter, public :: K_CMD_RELEASE_DEPARTURE = 62_int16
      !! Player command: Let a held departure go.
   integer(int16), parameter, public :: K_CMD_SEQUENCE_ARRIVAL = 63_int16
      !! Player command: Move an arrival to a place in the landing order.
   integer(int16), parameter, public :: K_CMD_SEQUENCE_DEPARTURE = 64_int16
      !! Player command: Move a departure to a place in the queue.
   integer(int16), parameter, public :: K_CMD_CLOSE_RUNWAY = 65_int16
      !! Player command: Take a runway out of use.
   integer(int16), parameter, public :: K_CMD_OPEN_RUNWAY = 66_int16
      !! Player command: Return a runway to use.
   integer(int16), parameter, public :: K_CMD_SET_ARRIVAL_RATE = 67_int16
      !! Player command: Set the declared arrival rate, per hour.

   integer, parameter :: MAX_EVENT_KIND = 67
      !! Largest identifier in use, and therefore the upper bound of the bus
      !! dispatch table. Default integer kind on purpose: it is an array bound,
      !! not simulation state. Recomputed by fypp whenever the list grows.

   integer, parameter :: N_EVENT_KINDS = 33
      !! How many kinds are declared, which is not `MAX_EVENT_KIND` because the
      !! numbering leaves gaps between sections.

contains

   pure function event_name(kind) result(name)
      !! Human-readable name of an event kind, for logs and queue dumps.
      !!
      !! An unknown kind is not an error: a log written by a newer build can
      !! still be read by an older one, and "Unknown" is more useful than a
      !! crash.
      integer(int16), intent(in) :: kind
         !! Identifier to look up.
      character(len=:), allocatable :: name

      select case (kind)
      case (1_int16)
         name = "Touchdown"
      case (2_int16)
         name = "RolloutComplete"
      case (3_int16)
         name = "RunwayExited"
      case (4_int16)
         name = "TaxiNodeReached"
      case (5_int16)
         name = "OnBlocks"
      case (6_int16)
         name = "TurnaroundComplete"
      case (7_int16)
         name = "PushbackRequested"
      case (8_int16)
         name = "PushbackComplete"
      case (9_int16)
         name = "LineUpCleared"
      case (10_int16)
         name = "TakeoffRollComplete"
      case (11_int16)
         name = "BingoFuel"
      case (12_int16)
         name = "HoldEntered"
      case (13_int16)
         name = "CapacityChanged"
      case (14_int16)
         name = "WeatherChanged"
      case (15_int16)
         name = "GateAssigned"
      case (16_int16)
         name = "GateReleased"
      case (17_int16)
         name = "ReplanRequested"
      case (18_int16)
         name = "IncidentTriggered"
      case (19_int16)
         name = "LineUpRequested"
      case (40_int16)
         name = "PassengerEnteredConcourse"
      case (41_int16)
         name = "PassengerExitedArrivals"
      case (42_int16)
         name = "PurchaseMade"
      case (43_int16)
         name = "CurbQueueChanged"
      case (44_int16)
         name = "BagLoaded"
      case (45_int16)
         name = "BagStolen"
      case (60_int16)
         name = "CmdAssignGate"
      case (61_int16)
         name = "CmdHoldDeparture"
      case (62_int16)
         name = "CmdReleaseDeparture"
      case (63_int16)
         name = "CmdSequenceArrival"
      case (64_int16)
         name = "CmdSequenceDeparture"
      case (65_int16)
         name = "CmdCloseRunway"
      case (66_int16)
         name = "CmdOpenRunway"
      case (67_int16)
         name = "CmdSetArrivalRate"
      case default
         name = "Unknown"
      end select
   end function event_name

end module core_event_kinds
