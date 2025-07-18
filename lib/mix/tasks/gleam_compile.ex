defmodule Mix.Tasks.GleamCompile do
  @shortdoc "Compiles Gleam files and copies them to the correct location"

  @moduledoc false
  use Mix.Task

  def run(_args) do
    env = Mix.env()

    # Check if compile.gleam task exists
    task_exists = Code.ensure_loaded?(Mix.Tasks.Compile.Gleam)

    if task_exists do
      # First run the gleam compiler
      Mix.Task.run("compile.gleam")

      # Ensure output directory exists
      ebin_dir = "_build/#{env}/lib/aprsme/ebin"
      File.mkdir_p!(ebin_dir)

      # Copy beam files from Gleam build to Elixir build
      gleam_output = "build/#{env}/erlang/aprsme/_gleam_artefacts"

      if File.exists?(gleam_output) do
        gleam_output
        |> File.ls!()
        |> Enum.filter(&String.ends_with?(&1, ".beam"))
        |> Enum.each(fn beam_file ->
          src = Path.join(gleam_output, beam_file)
          dest = Path.join(ebin_dir, beam_file)
          File.copy!(src, dest)
          Mix.shell().info("Copied #{beam_file}")
        end)
      end
    else
      Mix.shell().info("Gleam compiler not available, compiling manually")

      # Compile Gleam files manually using gleam binary if available
      if System.find_executable("gleam") do
        Mix.shell().info("Found gleam binary, compiling...")
        {output, _exit_code} = System.cmd("gleam", ["build"], cd: File.cwd!())
        Mix.shell().info(output)

        # Copy from gleam build to elixir build
        # Gleam always builds to dev directory regardless of MIX_ENV
        # The compiled files go to the project's ebin directory
        gleam_output = "build/dev/erlang/aprsme/ebin"
        ebin_dir = "_build/#{env}/lib/aprsme/ebin"
        File.mkdir_p!(ebin_dir)

        # First, check if compile.gleam already created the aprsme@encoding.beam
        dev_ebin = "_build/dev/lib/aprsme/ebin"

        if File.exists?(Path.join(dev_ebin, "aprsme@encoding.beam")) do
          Mix.shell().info("Found pre-compiled aprsme@encoding.beam")
          src = Path.join(dev_ebin, "aprsme@encoding.beam")
          dest = Path.join(ebin_dir, "aprsme@encoding.beam")
          File.copy!(src, dest)
          Mix.shell().info("Copied aprsme@encoding.beam to test build")
        end

        if File.exists?(gleam_output) do
          Mix.shell().info("Checking #{gleam_output} for beam files...")

          beam_files =
            gleam_output
            |> File.ls!()
            |> Enum.filter(&String.ends_with?(&1, ".beam"))

          Mix.shell().info("Found #{length(beam_files)} beam files")

          Enum.each(beam_files, fn beam_file ->
            src = Path.join(gleam_output, beam_file)
            dest = Path.join(ebin_dir, beam_file)
            File.copy!(src, dest)
            Mix.shell().info("Copied #{beam_file} to #{dest}")
          end)
        else
          Mix.shell().info("Gleam output directory #{gleam_output} does not exist")
        end
      else
        # Last resort: Check for pre-compiled BEAM files in priv/gleam
        Mix.shell().info("No Gleam compiler available, checking for pre-compiled BEAM files...")
        
        priv_gleam = "priv/gleam"
        ebin_dir = "_build/#{env}/lib/aprsme/ebin"
        
        if File.exists?(priv_gleam) do
          File.mkdir_p!(ebin_dir)
          
          # Look specifically for our Gleam module
          beam_file = "aprsme@encoding.beam"
          src = Path.join(priv_gleam, beam_file)
          
          if File.exists?(src) do
            dest = Path.join(ebin_dir, beam_file)
            File.copy!(src, dest)
            Mix.shell().info("Copied pre-compiled #{beam_file} from priv/gleam")
          else
            Mix.shell().error("Pre-compiled #{beam_file} not found in priv/gleam")
          end
        else
          # Try to copy from dev build if available
          dev_ebin = "_build/dev/lib/aprsme/ebin"
          
          if File.exists?(dev_ebin) and env != :dev do
            File.mkdir_p!(ebin_dir)
            
            dev_ebin
            |> File.ls!()
            |> Enum.filter(&String.starts_with?(&1, "aprsme@"))
            |> Enum.each(fn beam_file ->
              src = Path.join(dev_ebin, beam_file)
              dest = Path.join(ebin_dir, beam_file)
              File.copy!(src, dest)
              Mix.shell().info("Copied #{beam_file} from dev build")
            end)
          else
            Mix.shell().error("Unable to find Gleam compiled files. Please ensure Gleam code is compiled before deployment.")
          end
        end
      end
    end

    :ok
  end
end
