defmodule Aprsme.ErrorNotifierTest do
  use Aprsme.DataCase, async: false

  import Swoosh.TestAssertions

  alias Aprsme.ErrorNotifier
  alias Swoosh.Adapters.Test

  setup do
    # Store original config
    original_env = Application.get_env(:aprsme, :env)
    original_mailer = Application.get_env(:aprsme, Aprsme.Mailer)

    on_exit(fn ->
      Application.put_env(:aprsme, :env, original_env)
      Application.put_env(:aprsme, Aprsme.Mailer, original_mailer)
    end)

    :ok
  end

  describe "handle_error_occurrence/4" do
    test "sends email for first occurrence of an error in production" do
      # Set environment to production
      Application.put_env(:aprsme, :env, :prod)
      Application.put_env(:aprsme, Aprsme.Mailer, adapter: Test)

      occurrence_data = %{
        error_id: "test-error-123",
        reason: "RuntimeError: Something went wrong",
        occurrence_count: 1,
        stacktrace: %{
          lines: [
            %{
              module: "Aprsme.SomeModule",
              function: "some_function/2",
              file: "lib/aprsme/some_module.ex",
              line: 42
            }
          ]
        },
        context: %{
          "live_view.view" => "AprsmeWeb.MapLive.Index",
          "request.path" => "/map"
        }
      }

      measurements = %{}
      metadata = %{occurrence: occurrence_data}

      ErrorNotifier.handle_error_occurrence(
        [:error_tracker, :occurrence, :created],
        measurements,
        metadata,
        nil
      )

      assert_email_sent(fn email ->
        assert email.to == [{"", "graham@mcintire.me"}]
        assert email.subject =~ "Something went wrong"
        assert email.subject =~ "lib/aprsme/some_module.ex"
        assert email.subject =~ "42"
        assert email.html_body =~ "test-error-123"
        assert email.html_body =~ "RuntimeError: Something went wrong"
        assert email.html_body =~ "Aprsme.SomeModule.some_function/2"
        assert email.html_body =~ "https://aprs.me/dev/error-tracker/errors/test-error-123"
      end)
    end

    test "does not send email for subsequent occurrences" do
      Application.put_env(:aprsme, :env, :prod)
      Application.put_env(:aprsme, Aprsme.Mailer, adapter: Test)

      occurrence_data = %{
        error_id: "test-error-123",
        reason: "RuntimeError: Something went wrong",
        occurrence_count: 5,
        stacktrace: %{lines: []},
        context: %{}
      }

      measurements = %{}
      metadata = %{occurrence: occurrence_data}

      ErrorNotifier.handle_error_occurrence(
        [:error_tracker, :occurrence, :created],
        measurements,
        metadata,
        nil
      )

      assert_no_email_sent()
    end

    test "does not send email in development environment" do
      Application.put_env(:aprsme, :env, :dev)
      Application.put_env(:aprsme, Aprsme.Mailer, adapter: Test)

      occurrence_data = %{
        error_id: "test-error-123",
        reason: "RuntimeError: Something went wrong",
        occurrence_count: 1,
        stacktrace: %{lines: []},
        context: %{}
      }

      measurements = %{}
      metadata = %{occurrence: occurrence_data}

      ErrorNotifier.handle_error_occurrence(
        [:error_tracker, :occurrence, :created],
        measurements,
        metadata,
        nil
      )

      assert_no_email_sent()
    end

    test "does not send email in test environment" do
      Application.put_env(:aprsme, :env, :test)
      Application.put_env(:aprsme, Aprsme.Mailer, adapter: Test)

      occurrence_data = %{
        error_id: "test-error-123",
        reason: "RuntimeError: Something went wrong",
        occurrence_count: 1,
        stacktrace: %{lines: []},
        context: %{}
      }

      measurements = %{}
      metadata = %{occurrence: occurrence_data}

      ErrorNotifier.handle_error_occurrence(
        [:error_tracker, :occurrence, :created],
        measurements,
        metadata,
        nil
      )

      assert_no_email_sent()
    end

    test "handles missing stacktrace gracefully" do
      Application.put_env(:aprsme, :env, :prod)
      Application.put_env(:aprsme, Mailer, adapter: Test)

      occurrence_data = %{
        error_id: "test-error-123",
        reason: "RuntimeError: Something went wrong",
        occurrence_count: 1,
        stacktrace: %{lines: []},
        context: %{}
      }

      measurements = %{}
      metadata = %{occurrence: occurrence_data}

      ErrorNotifier.handle_error_occurrence(
        [:error_tracker, :occurrence, :created],
        measurements,
        metadata,
        nil
      )

      assert_email_sent(fn email ->
        assert email.subject =~ "unknown_file"
        assert email.html_body =~ "Unknown location"
      end)
    end
  end
end
