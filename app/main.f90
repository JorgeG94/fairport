! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! fairport: deterministic airport operations simulator.
program fairport
   !! Batch mode only at milestone 0. Sim time is already decoupled from wall
   !! time -- there is no pacing at all -- so a scenario runs as fast as the
   !! machine can dispatch events, which is the mode that matters: running two
   !! hundred seeds and histogramming the result is how anyone finds out
   !! whether the rules produce interesting decisions or noise.
   use core_sim, only: sim_t
   use app_scenario, only: run_scenario
   use pic_types, only: int64
   use pic_error, only: error_t
   use pic_cli, only: cli_t
   use pic_logger, only: logger => global_logger, error_level, info_level
   implicit none

   type(sim_t), target :: sim
      !! **`target` is required.** The bus holds pointers to the systems inside
      !! `sim`, and a component of a non-target object is not a valid pointer
      !! target.
   type(cli_t) :: cli
   type(error_t) :: err
   character(len=:), allocatable :: scenario_path
   integer(int64) :: seed
   logical :: want_hash, want_board, has_seed

   call cli%set_program("fairport", "Deterministic airport operations simulator")
   call cli%add_positional("scenario", "Scenario script to run", required=.true.)
   call cli%add_option("seed", "Master RNG seed, overriding the script", short="s", default="0")
   call cli%add_flag("hash", "Print only the event log hash", short="H")
   call cli%add_flag("board", "Draw the ops board when the run ends", short="b")

   call cli%parse(err)

   if (cli%help_requested()) then
      call logger%info(cli%help_text())
      stop
   end if

   if (err%has_error()) then
      call logger%error(err%get_message())
      call logger%info(cli%help_text())
      stop 2
   end if

   call cli%get("scenario", scenario_path, err)
   if (err%has_error()) call fail(err)

   call cli%get("seed", seed, err)
   if (err%has_error()) call fail(err)
   has_seed = cli%is_set("seed")

   want_hash = cli%is_set("hash")
   want_board = cli%is_set("board")

   ! `--hash` is what the cross-compiler CI job compares, so nothing else may
   ! reach the console in that mode.
   if (want_hash) call logger%configure(error_level)

   call sim%init(seed, err)
   if (err%has_error()) call fail(err)

   call run_scenario(sim, scenario_path, seed, has_seed, want_board, err)
   if (err%has_error()) call fail(err)

   if (want_hash) then
      call logger%configure(info_level)
      call logger%info(sim%log%digest_hex())
   end if

   call sim%destroy()

contains

   subroutine fail(failure)
      !! Report and exit non-zero.
      type(error_t), intent(inout) :: failure
         !! The error to report.

      call logger%configure(info_level)
      call logger%error(failure%get_message())
      call sim%destroy()
      stop 1
   end subroutine fail

end program fairport
