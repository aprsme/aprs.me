defmodule AprsmeWeb.ThemeManager do
  @moduledoc """
  Server-side theme management for better performance and consistency.

  Moves theme logic from JavaScript to LiveView to:
  - Reduce client-side complexity
  - Provide server-side theme state management
  - Enable theme persistence across page loads
  - Improve performance by eliminating JavaScript theme detection
  """

  use Phoenix.Component

  @themes ~w(light dark auto)
  @default_theme "auto"

  @doc """
  Initializes theme from session/user preferences.
  Call this from your LiveView's mount/3 callback.
  """
  def init_theme(socket, session \\ %{}) do
    theme = get_stored_theme(session)
    resolved_theme = resolve_theme(theme, get_system_preference())

    Phoenix.Component.assign(socket, %{
      theme: theme,
      resolved_theme: resolved_theme,
      theme_colors: get_theme_colors(resolved_theme)
    })
  end

  @doc """
  Handles theme change events from the client.
  Call this from your LiveView's handle_event/3 callback.
  """
  def handle_theme_change(socket, theme) when theme in @themes do
    resolved_theme = resolve_theme(theme, get_system_preference())

    # Store theme preference (could be extended to user preferences)
    # For now, we'll use session storage

    socket
    |> Phoenix.Component.assign(%{
      theme: theme,
      resolved_theme: resolved_theme,
      theme_colors: get_theme_colors(resolved_theme)
    })
    |> Phoenix.LiveView.push_event("update_theme", %{
      theme: theme,
      resolved_theme: resolved_theme,
      colors: get_theme_colors(resolved_theme)
    })
  end

  def handle_theme_change(socket, _invalid_theme), do: socket

  @doc """
  Resolves 'auto' theme based on system preference.
  """
  def resolve_theme("auto", system_preference), do: system_preference
  def resolve_theme(theme, _system_preference) when theme in ["light", "dark"], do: theme
  def resolve_theme(_invalid, system_preference), do: system_preference

  @doc """
  Gets theme colors for charts and components.
  """
  def get_theme_colors("dark") do
    %{
      text: "#e5e7eb",
      grid: "#374151",
      background: "rgba(0, 0, 0, 0.1)",
      primary: "#3b82f6",
      secondary: "#8b5cf6",
      accent: "#06b6d4"
    }
  end

  def get_theme_colors(_light_or_other) do
    %{
      text: "#111827",
      grid: "#9ca3af",
      background: "rgba(255, 255, 255, 0.1)",
      primary: "#2563eb",
      secondary: "#7c3aed",
      accent: "#0891b2"
    }
  end

  # Gets stored theme from session or defaults to auto.
  defp get_stored_theme(%{"theme" => theme}) when theme in @themes, do: theme
  defp get_stored_theme(_session), do: @default_theme

  # Gets system theme preference (defaults to light for server-side).
  # In a real implementation, this could be determined from user agent
  # or stored in user preferences.
  defp get_system_preference, do: "light"

  @doc """
  Component for theme selector.
  """
  attr :theme, :string, required: true
  attr :class, :string, default: ""

  def theme_selector(assigns) do
    ~H"""
    <div class={@class}>
      <div class="flex rounded-lg bg-gray-100 dark:bg-gray-800 p-1">
        <button
          type="button"
          phx-click="set_theme"
          phx-value-theme="light"
          class={[
            "flex items-center gap-2 rounded-md px-3 py-1.5 text-sm font-medium transition-colors",
            if(@theme == "light",
              do: "bg-white dark:bg-gray-900 text-gray-900 dark:text-white shadow-sm",
              else: "text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-white"
            )
          ]}
        >
          <svg class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"
            />
          </svg>
          Light
        </button>

        <button
          type="button"
          phx-click="set_theme"
          phx-value-theme="dark"
          class={[
            "flex items-center gap-2 rounded-md px-3 py-1.5 text-sm font-medium transition-colors",
            if(@theme == "dark",
              do: "bg-white dark:bg-gray-900 text-gray-900 dark:text-white shadow-sm",
              else: "text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-white"
            )
          ]}
        >
          <svg class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"
            />
          </svg>
          Dark
        </button>

        <button
          type="button"
          phx-click="set_theme"
          phx-value-theme="auto"
          class={[
            "flex items-center gap-2 rounded-md px-3 py-1.5 text-sm font-medium transition-colors",
            if(@theme == "auto",
              do: "bg-white dark:bg-gray-900 text-gray-900 dark:text-white shadow-sm",
              else: "text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-white"
            )
          ]}
        >
          <svg class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"
            />
          </svg>
          Auto
        </button>
      </div>
    </div>
    """
  end
end
