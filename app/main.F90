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
   use app_scenario, only: run_scenario, save_session, session_t
#ifdef FAIRPORT_TUI
   use app_tui, only: run_interactive_scenario
#endif
   use pic_types, only: int64
   use pic_error, only: error_t
#ifndef FAIRPORT_TUI
   ! Only the no-terminal build refuses `--play`, so only it needs the code.
   use pic_error, only: ERROR_VALIDATION
#endif
   use pic_cli, only: cli_t
   use pic_logger, only: logger => global_logger, error_level, info_level
   implicit none

   type(sim_t), target :: sim
      !! **`target` is required.** The bus holds pointers to the systems inside
      !! `sim`, and a component of a non-target object is not a valid pointer
      !! target.
   type(cli_t) :: cli
   type(error_t) :: err
   type(session_t) :: session
      !! The world half of the script, kept so `--save` can write it back out.
   character(len=:), allocatable :: scenario_path, save_path
   integer(int64) :: seed
   logical :: want_hash, want_board, has_seed, want_save

   call cli%set_program("fairport", "Deterministic airport operations simulator")
   call cli%add_positional("scenario", "Scenario script to run", required=.true.)
   call cli%add_option("seed", "Master RNG seed, overriding the script", short="s", default="0")
   call cli%add_flag("hash", "Print only the event log hash", short="H")
   call cli%add_flag("board", "Draw the ops board when the run ends", short="b")
   call cli%add_flag("play", "Watch the day on an interactive board", short="p")
   call cli%add_option("save", "Write the session out as a replayable scenario", default="")

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

   want_save = cli%is_set("save")
   call cli%get("save", save_path, err)
   if (err%has_error()) call fail(err)

   ! `--hash` is what the cross-compiler CI job compares, so nothing else may
   ! reach the console in that mode.
   if (want_hash) call logger%configure(error_level)

   call sim%init(seed, err)
   if (err%has_error()) call fail(err)

   if (cli%is_set("play")) then
      call play(sim, scenario_path, seed, has_seed, session, err)
   else
      call run_scenario(sim, scenario_path, seed, has_seed, want_board, err, session)
   end if
   if (err%has_error()) call fail(err)

   ! Saved after the run rather than during it, because the log is only whole
   ! once the day has stopped -- and because a save written from inside the
   ! frame loop would put file I/O on the path that has to keep 2 Hz.
   if (want_save) then
      call save_session(sim, session, save_path, err)
      if (err%has_error()) call fail(err)
   end if

   if (want_hash) then
      call logger%configure(info_level)
      call logger%info(sim%log%digest_hex())
   end if

   call sim%destroy()

contains

   subroutine play(simulation, path, master_seed, seeded, played, failure)
      !! Load a scenario and watch it, rather than running it to completion.
      !!
      !! The scenario is read with its `run until` lines honoured as the
      !! horizon; what changes is that time advances against the wall clock
      !! instead of as fast as the queue can be drained.
      type(sim_t), target, intent(inout) :: simulation
         !! Simulation to drive.
      character(len=*), intent(in) :: path
         !! Scenario to load.
      integer(int64), intent(in) :: master_seed
         !! Seed from the command line.
      logical, intent(in) :: seeded
         !! Whether the command line supplied one.
      type(session_t), intent(out) :: played
         !! The world half of the script, for `--save`.
      type(error_t), intent(inout) :: failure

#ifdef FAIRPORT_TUI
      call run_interactive_scenario(simulation, path, master_seed, seeded, failure, played)
#else
      call failure%set(ERROR_VALIDATION, &
                       "this build has no interactive board; configure with -DFAIRPORT_ENABLE_TUI=ON")
#endif
   end subroutine play

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
