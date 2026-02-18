defmodule Mix.Tasks.Aprs.ParseFile do
  @shortdoc "Parse APRS packets from a file and report failures"

  @moduledoc """
  Reads APRS packets from a text file, attempts to parse each one, and writes
  any failures to an output file.

  ## Usage

      mix aprs.parse_file [input_file] [--output output_file]

  ## Options

    * `--output` / `-o` - path for the output file (default: `bad_packets.txt`)

  ## Arguments

    * `input_file` - path to input file (default: `packets.txt`)

  Lines starting with `#` or blank lines are skipped.
  """

  use Mix.Task

  @default_input "packets.txt"
  @default_output "bad_packets.txt"

  @impl Mix.Task
  def run(args) do
    {opts, positional, _} =
      OptionParser.parse(args,
        aliases: [o: :output],
        strict: [output: :string]
      )

    input_file = List.first(positional) || @default_input
    output_file = Keyword.get(opts, :output, @default_output)

    if !File.exists?(input_file) do
      Mix.shell().error("Error: input file not found: #{input_file}")
      exit({:shutdown, 1})
    end

    Application.put_env(:aprsme, :disable_aprs_connection, true)
    Mix.Task.run("app.start")

    {total, failures} =
      input_file
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Stream.reject(&skip_line?/1)
      |> Enum.reduce({0, []}, fn line, {count, bad} ->
        case Aprs.parse(line) do
          {:ok, _} -> {count + 1, bad}
          {:error, _} -> {count + 1, [line | bad]}
        end
      end)

    failures = Enum.reverse(failures)
    pass_count = total - length(failures)
    fail_count = length(failures)

    File.write!(output_file, Enum.join(failures, "\n") <> if(failures == [], do: "", else: "\n"))

    Mix.shell().info("Processed #{total} packets: #{pass_count} passed, #{fail_count} failed")
    Mix.shell().info("Output written to: #{output_file}")
  end

  defp skip_line?(""), do: true
  defp skip_line?("#" <> _), do: true
  defp skip_line?(_), do: false
end
