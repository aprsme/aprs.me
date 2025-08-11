This is a web application written using the Phoenix web framework.

## Elixir guidelines

- Elixir lists **do not support index based access via the access syntax**

  **Never do this (invalid)**:

      i = 0
      mylist = ["blue", "green"]
      mylist[i]

  Instead, **always** use `Enum.at`, pattern matching, or `List` for index based list access, ie:

      i = 0
      mylist = ["blue", "green"]
      Enum.at(mylist, i)

- Elixir supports `if/else` but **does NOT support `if/else if` or `if/elsif`. **Never use `else if` or `elseif` in Elixir**, **always** use `cond` or `case` for multiple conditionals.

  **Never do this (invalid)**:

      <%= if condition do %>
        ...
      <% else if other_condition %>
        ...
      <% end %>

  Instead **always** do this:

      <%= cond do %>
        <% condition -> %>
          ...
        <% condition2 -> %>
          ...
        <% true -> %>
          ...
      <% end %>

- Elixir variables are immutable, but can be rebound, so for block expressions like `if`, `case`, `cond`, etc
  you *must* bind the result of the expression to a variable if you want to use it and you CANNOT rebind the result inside the expression, ie:

      # INVALID: we are rebinding inside the `if` and the result never gets assigned
      if connected?(socket) do
        socket = assign(socket, :val, val)
      end

      # VALID: we rebind the result of the `if` to a new variable
      socket =
        if connected?(socket) do
          assign(socket, :val, val)
        end

- Use `with` for chaining operations that return `{:ok, _}` or `{:error, _}`
- **Never** nest multiple modules in the same file as it can cause cyclic dependencies and compilation errors
- **Never** use map access syntax (`changeset[:field]`) on structs as they do not implement the Access behaviour by default. For regular structs, you **must** access the fields directly, such as `my_struct.field` or use higher level APIs that are available on the struct if they exist, `Ecto.Changeset.get_field/2` for changesets
- Elixir's standard library has everything necessary for date and time manipulation. Familiarize yourself with the common `Time`, `Date`, `DateTime`, and `Calendar` interfaces by accessing their documentation as necessary. **Never** install additional dependencies unless asked or for date/time parsing (which you can use the `date_time_parser` package)
- Don't use `String.to_atom/1` on user input (memory leak risk)
- Predicate function names should not start with `is_` and should end in a question mark. Names like `is_thing` should be reserved for guards
- Elixir's builtin OTP primitives like `DynamicSupervisor` and `Registry`, require names in the child spec, such as `{DynamicSupervisor, name: MyApp.MyDynamicSup}`, then you can use `DynamicSupervisor.start_child(MyApp.MyDynamicSup, child_spec)`
- Use `Task.async_stream(collection, callback, options)` for concurrent enumeration with back-pressure. The majority of times you will want to pass `timeout: :infinity` as option

## Mix guidelines

- Read the docs and options before using tasks (by using `mix help task_name`)
- To debug test failures, run tests in a specific file with `mix test test/my_test.exs` or run all previously failed tests with `mix test --failed`
- `mix deps.clean --all` is **almost never needed**. **Avoid** using it unless you have good reason

## Phoenix guidelines

- Remember Phoenix router `scope` blocks include an optional alias which is prefixed for all routes within the scope. **Always** be mindful of this when creating routes within a scope to avoid duplicate module prefixes.

- You **never** need to create your own `alias` for route definitions! The `scope` provides the alias, ie:

      scope "/admin", AppWeb.Admin do
        pipe_through :browser

        live "/users", UserLive, :index
      end

  the UserLive route would point to the `AppWeb.Admin.UserLive` module

- `Phoenix.View` no longer is needed or included with Phoenix, don't use it

## Ecto Guidelines

- **Always** preload Ecto associations in queries when they'll be accessed in templates, ie a message that needs to reference the `message.user.email`
- Remember `import Ecto.Query` and other supporting modules when you write `seeds.exs`
- `Ecto.Schema` fields always use the `:string` type, even for `:text`, columns, ie: `field :name, :string`
- `Ecto.Changeset.validate_number/2` **DOES NOT SUPPORT the `:allow_nil` option**. By default, Ecto validations only run if a change for the given field exists and the change value is not nil, so such as option is never needed
- You **must** use `Ecto.Changeset.get_field(changeset, :field)` to access changeset fields
- Fields which are set programatically, such as `user_id`, must not be listed in `cast` calls or similar for security purposes. Instead they must be explicitly set when creating the struct

## Phoenix HTML guidelines

- Phoenix templates **always** use `~H` or .html.heex files (known as HEEx), **never** use `~E`
- **Always** use the imported `Phoenix.Component.form/1` and `Phoenix.Component.inputs_for/1` function to build forms. **Never** use `Phoenix.HTML.form_for` or `Phoenix.HTML.inputs_for` as they are outdated
- When building forms **always** use the already imported `Phoenix.Component.to_form/2` (`assign(socket, form: to_form(...))` and `<.form for={@form} id="msg-form">`), then access those forms in the template via `@form[:field]`
- **Always** add unique DOM IDs to key elements (like forms, buttons, etc) when writing templates, these IDs can later be used in tests (`<.form for={@form} id="product-form">`)
- For "app wide" template imports, you can import/alias into the `my_app_web.ex`'s `html_helpers` block, so they will be available to all LiveViews, LiveComponent's, and all modules that do `use MyAppWeb, :html` (replace "my_app" by the actual app name)

- HEEx require special tag annotation if you want to insert literal curly's like `{` or `}`. If you want to show a textual code snippet on the page in a `<pre>` or `<code>` block you *must* annotate the parent tag with `phx-no-curly-interpolation`:

      <code phx-no-curly-interpolation>
        let obj = {key: "val"}
      </code>

  Within `phx-no-curly-interpolation` annotated tags, you can use `{` and `}` without escaping them, and dynamic Elixir expressions can still be used with `<%= ... %>` syntax

- HEEx class attrs support lists, but you must **always** use list `[...]` syntax. You can use the class list syntax to conditionally add classes, **always do this for multiple class values**:

      <a class={[
        "px-2 text-white",
        @some_flag && "py-5",
        if(@other_condition, do: "border-red-500", else: "border-blue-100"),
        ...
      ]}>Text</a>

  and **always** wrap `if`'s inside `{...}` expressions with parens, like done above (`if(@other_condition, do: "...", else: "...")`)

  and **never** do this, since it's invalid (note the missing `[` and `]`):

      <a class={
        "px-2 text-white",
        @some_flag && "py-5"
      }> ...
      => Raises compile syntax error on invalid HEEx attr syntax

- **Never** use `<% Enum.each %>` or non-for comprehensions for generating template content, instead **always** use `<%= for item <- @collection do %>`
- HEEx HTML comments use `<%!-- comment --%>`. **Always** use the HEEx HTML comment syntax for template comments (`<%!-- comment --%>`)
- HEEx allows interpolation via `{...}` and `<%= ... %>`, but the `<%= %>` **only** works within tag bodies. **Always** use the `{...}` syntax for interpolation within tag attributes, and for interpolation of values within tag bodies. **Always** interpolate block constructs (if, cond, case, for) within tag bodies using `<%= ... %>`.

  **Always** do this:

      <div id={@id}>
        {@my_assign}
        <%= if @some_block_condition do %>
          {@another_assign}
        <% end %>
      </div>

  and **Never** do this – the program will terminate with a syntax error:

      <%!-- THIS IS INVALID NEVER EVER DO THIS --%>
      <div id="<%= @invalid_interpolation %>">
        {if @invalid_block_construct do}
        {end}
      </div>

## Phoenix LiveView guidelines

- **Never** use the deprecated `live_redirect` and `live_patch` functions, instead **always** use the `<.link navigate={href}>` and  `<.link patch={href}>` in templates, and `push_navigate` and `push_patch` functions LiveViews
- **Avoid LiveComponent's** unless you have a strong, specific need for them
- LiveViews should be named like `AppWeb.WeatherLive`, with a `Live` suffix. When you go to add LiveView routes to the router, the default `:browser` scope is **already aliased** with the `AppWeb` module, so you can just do `live "/weather", WeatherLive`
- Remember anytime you use `phx-hook="MyHook"` and that js hook manages its own DOM, you **must** also set the `phx-update="ignore"` attribute
- **Never** write embedded `<script>` tags in HEEx. Instead always write your scripts and hooks in the `assets/js` directory and integrate them with the `assets/js/app.js` file

### LiveView streams

- **Always** use LiveView streams for collections for assigning regular lists to avoid memory ballooning and runtime termination with the following operations:
  - basic append of N items - `stream(socket, :messages, [new_msg])`
  - resetting stream with new items - `stream(socket, :messages, [new_msg], reset: true)` (e.g. for filtering items)
  - prepend to stream - `stream(socket, :messages, [new_msg], at: -1)`
  - deleting items - `stream_delete(socket, :messages, msg)`

- When using the `stream/3` interfaces in the LiveView, the LiveView template must 1) always set `phx-update="stream"` on the parent element, with a DOM id on the parent element like `id="messages"` and 2) consume the `@streams.stream_name` collection and use the id as the DOM id for each child. For a call like `stream(socket, :messages, [new_msg])` in the LiveView, the template would be:

      <div id="messages" phx-update="stream">
        <div :for={{id, msg} <- @streams.messages} id={id}>
          {msg.text}
        </div>
      </div>

- LiveView streams are *not* enumerable, so you cannot use `Enum.filter/2` or `Enum.reject/2` on them. Instead, if you want to filter, prune, or refresh a list of items on the UI, you **must refetch the data and re-stream the entire stream collection, passing reset: true**:

      def handle_event("filter", %{"filter" => filter}, socket) do
        # re-fetch the messages based on the filter
        messages = list_messages(filter)

        {:noreply,
        socket
        |> assign(:messages_empty?, messages == [])
        # reset the stream with the new messages
        |> stream(:messages, messages, reset: true)}
      end

- LiveView streams *do not support counting or empty states*. If you need to display a count, you must track it using a separate assign. For empty states, you can use Tailwind classes:

      <div id="tasks" phx-update="stream">
        <div class="hidden only:block">No tasks yet</div>
        <div :for={{id, task} <- @stream.tasks} id={id}>
          {task.name}
        </div>
      </div>

  The above only works if the empty state is the only HTML block alongside the stream for-comprehension.

- **Never** use the deprecated `phx-update="append"` or `phx-update="prepend"` for collections

### LiveView tests

- `Phoenix.LiveViewTest` module and `LazyHTML` (included) for making your assertions
- Form tests are driven by `Phoenix.LiveViewTest`'s `render_submit/2` and `render_change/2` functions
- Come up with a step-by-step test plan that splits major test cases into small, isolated files. You may start with simpler tests that verify content exists, gradually add interaction tests
- **Always reference the key element IDs you added in the LiveView templates in your tests** for `Phoenix.LiveViewTest` functions like `element/2`, `has_element/2`, selectors, etc
- **Never** tests again raw HTML, **always** use `element/2`, `has_element/2`, and similar: `assert has_element?(view, "#my-form")`
- Instead of relying on testing text content, which can change, favor testing for the presence of key elements
- Focus on testing outcomes rather than implementation details
- Be aware that `Phoenix.Component` functions like `<.form>` might produce different HTML than expected. Test against the output HTML structure, not your mental model of what you expect it to be
- When facing test failures with element selectors, add debug statements to print the actual HTML, but use `LazyHTML` selectors to limit the output, ie:

      html = render(view)
      document = LazyHTML.from_fragment(html)
      matches = LazyHTML.filter(document, "your-complex-selector")
      IO.inspect(matches, label: "Matches")

### Form handling

#### Creating a form from params

If you want to create a form based on `handle_event` params:

    def handle_event("submitted", params, socket) do
      {:noreply, assign(socket, form: to_form(params))}
    end

When you pass a map to `to_form/1`, it assumes said map contains the form params, which are expected to have string keys.

You can also specify a name to nest the params:

    def handle_event("submitted", %{"user" => user_params}, socket) do
      {:noreply, assign(socket, form: to_form(user_params, as: :user))}
    end

#### Creating a form from changesets

When using changesets, the underlying data, form params, and errors are retrieved from it. The `:as` option is automatically computed too. E.g. if you have a user schema:

    defmodule MyApp.Users.User do
      use Ecto.Schema
      ...
    end

And then you create a changeset that you pass to `to_form`:

    %MyApp.Users.User{}
    |> Ecto.Changeset.change()
    |> to_form()

Once the form is submitted, the params will be available under `%{"user" => user_params}`.

In the template, the form form assign can be passed to the `<.form>` function component:

    <.form for={@form} id="todo-form" phx-change="validate" phx-submit="save">
      <.input field={@form[:field]} type="text" />
    </.form>

Always give the form an explicit, unique DOM ID, like `id="todo-form"`.

#### Avoiding form errors

**Always** use a form assigned via `to_form/2` in the LiveView, and the `<.input>` component in the template. In the template **always access forms this**:

    <%!-- ALWAYS do this (valid) --%>
    <.form for={@form} id="my-form">
      <.input field={@form[:field]} type="text" />
    </.form>

And **never** do this:

    <%!-- NEVER do this (invalid) --%>
    <.form for={@changeset} id="my-form">
      <.input field={@changeset[:field]} type="text" />
    </.form>

- You are FORBIDDEN from accessing the changeset in the template as it will cause errors
- **Never** use `<.form let={f} ...>` in the template, instead **always use `<.form for={@form} ...>`**, then drive all form references from the form assign as in `@form[:field]`. The UI should **always** be driven by a `to_form/2` assigned in the LiveView module that is derived from a changeset

## Project guidelines

- Use `mix precommit` alias when you are done with all changes and fix any pending issues
- Use the already included and available `:req` (`Req`) library for HTTP requests, **avoid** `:httpoison`, `:tesla`, and `:httpc`. Req is included by default and is the preferred HTTP client for Phoenix apps

### Phoenix v1.8 guidelines

- **Always** begin your LiveView templates with `<Layouts.app flash={@flash} ...>` which wraps all inner content
- The `MyAppWeb.Layouts` module is aliased in the `my_app_web.ex` file, so you can use it without needing to alias it again
- Anytime you run into errors with no `current_scope` assign:
  - You failed to follow the Authenticated Routes guidelines, or you failed to pass `current_scope` to `<Layouts.app>`
  - **Always** fix the `current_scope` error by moving your routes to the proper `live_session` and ensure you pass `current_scope` as needed
- Phoenix v1.8 moved the `<.flash_group>` component to the `Layouts` module. You are **forbidden** from calling `<.flash_group>` outside of the `layouts.ex` module
- Out of the box, `core_components.ex` imports an `<.icon name="hero-x-mark" class="w-5 h-5"/>` component for for hero icons. **Always** use the `<.icon>` component for icons, **never** use `Heroicons` modules or similar
- **Always** use the imported `<.input>` component for form inputs from `core_components.ex` when available. `<.input>` is imported and using it will will save steps and prevent errors
- If you override the default input classes (`<.input class="myclass px-2 py-1 rounded-lg">)`) class with your own values, no default classes are inherited, so your
custom classes must fully style the input

### JS and CSS guidelines

- **Use Tailwind CSS classes and custom CSS rules** to create polished, responsive, and visually stunning interfaces.
- Tailwindcss v4 **no longer needs a tailwind.config.js** and uses a new import syntax in `app.css`:

      @import "tailwindcss" source(none);
      @source "../css";
      @source "../js";
      @source "../../lib/my_app_web";

- **Always use and maintain this import syntax** in the app.css file for projects generated with `phx.new`
- **Never** use `@apply` when writing raw css
- **Always** manually write your own tailwind-based components instead of using daisyUI for a unique, world-class design
- Out of the box **only the app.js and app.css bundles are supported**
  - You cannot reference an external vendor'd script `src` or link `href` in the layouts
  - You must import the vendor deps into app.js and app.css to use them
  - **Never write inline <script>custom js</script> tags within templates**

### UI/UX & design guidelines

- **Produce world-class UI designs** with a focus on usability, aesthetics, and modern design principles
- Implement **subtle micro-interactions** (e.g., button hover effects, and smooth transitions)
- Ensure **clean typography, spacing, and layout balance** for a refined, premium look
- Focus on **delightful details** like hover effects, loading states, and smooth page transitions

## Authentication

- **Always** handle authentication flow at the router level with proper redirects
- **Always** be mindful of where to place routes. `phx.gen.auth` creates multiple router plugs and `live_session` scopes:
  - A `live_session :current_user` scope - For routes that need the current user but don't require authentication
  - A `live_session :require_authenticated_user` scope - For routes that require authentication
  - In both cases, a `@current_scope` is assigned to the Plug connection and LiveView socket
- **Always let the user know in which router scopes, `live_session`, and pipeline you are placing the route, AND SAY WHY**
- `phx.gen.auth` assigns the `current_scope` assign - it **does not assign the `current_user` assign**.
- To derive/access `current_user`, **always use the `current_scope.user` assign**, never use **`@current_user`** in templates or LiveViews
- **Never** duplicate `live_session` names. A `live_session :current_user` can only be defined __once__ in the router, so all routes for the `live_session :current_user`  must be grouped in a single block
- Anytime you hit `current_scope` errors or the logged in session isn't displaying the right content, **always double check the router and ensure you are using the correct `live_session` described below**

### Routes that require authentication

LiveViews that require login should **always be placed inside the __existing__ `live_session :require_authenticated_user` block**:

    scope "/", AppWeb do
      pipe_through [:browser, :require_authenticated_user]

      live_session :require_authenticated_user,
        on_mount: [{AppWeb.UserAuth, :ensure_authenticated}] do
        # phx.gen.auth generated routes
        live "/users/settings", UserSettingsLive, :edit
        live "/users/settings/confirm_email/:token", UserSettingsLive, :confirm_email
        # our own routes that require logged in user
        live "/", MyLiveThatRequiresAuth, :index
      end
    end

### Routes that work with or without authentication

LiveViews that can work with or without authentication, **always use the __existing__ `:current_user` scope**, ie:

    scope "/", MyAppWeb do
      pipe_through [:browser]

      live_session :current_user,
        on_mount: [{MyAppWeb.UserAuth, :mount_current_scope}] do
        # our own routes that work with or without authentication
        live "/", PublicLive
      end
    end

## Prerequisites

Before setting up the project, ensure you have the following installed:
- Elixir 1.17+
- Erlang/OTP
- PostgreSQL with PostGIS extension

## Project Overview

This is an Elixir Phoenix LiveView application that serves as a real-time APRS (Automatic Packet Reporting System) tracker and visualizer. It connects to the APRS-IS network to receive live amateur radio packets and displays them on an interactive map interface.

## Development Commands

### Setup
- `mix setup` - Complete project setup (deps.get + ecto.setup)
- `mix deps.get` - Install dependencies
- `mix ecto.setup` - Create database, run migrations, and seed data
- `mix ecto.reset` - Drop and recreate database
- `mix phx.server` - Start Phoenix server (http://localhost:4000)
- `iex -S mix phx.server` - Start server in interactive Elixir shell

### Testing
- `mix test` - Run full test suite
- `mix test --stale` - Run only tests affected by code changes
- `mix test.watch` - Continuous testing with file watching
- `mix test --cover` - Generate test coverage reports

### Code Quality
- `mix format` - Format code according to .formatter.exs
- `mix credo` - Static code analysis and style checking
- `mix dialyzer` - Static type analysis (must run and fix errors/warnings)
- `mix sobelow` - Security vulnerability scanning
- **CRITICAL**: ALWAYS run `mix format` BEFORE committing - never commit unformatted code
- **MANDATORY**: Run `mix compile --warnings-as-errors` and ensure it passes before considering any task complete

### Assets (No Node.js Required)
- `mix assets.deploy` - Build and minify frontend assets (Tailwind CSS + ESBuild)
- Phoenix uses standalone ESBuild and Tailwind binaries - no npm/yarn needed
- JavaScript bundling handled by ESBuild
- CSS compilation handled by Tailwind CLI

## Architecture

### Core Components
- **Aprsme.AprsIsConnection** - TCP connection to APRS-IS network with reconnection logic
- **Aprsme.PacketConsumer** - Processes incoming APRS packets using GenStage pipeline
- **Aprsme.Packet** - Database schema for APRS packets with PostGIS geographic data
- **AprsmeWeb.MapLive.Index** - Main real-time map interface using Phoenix LiveView
- **Aprsme.Workers.PacketCleanupWorker** - Oban background job for data cleanup

### Data Flow
1. APRS-IS connection receives packets via TCP
2. PacketConsumer processes packets through GenStage pipeline
3. Packets stored in PostgreSQL with PostGIS geographic indexing
4. LiveView broadcasts real-time updates to connected clients via PubSub
5. Background workers handle cleanup and maintenance tasks

### Key Dependencies
- Phoenix LiveView for real-time UI without JavaScript
- PostGIS for geographic data storage and spatial queries
- Oban for background job processing
- GenStage for packet processing pipelines
- Tailwind CSS + ESBuild for frontend assets (no Node.js)

## Test-Driven Development

**MANDATORY**: Follow strict test-driven development (TDD) practices:

**CRITICAL**: When a new feature or bug is introduced, ALWAYS write a test for it FIRST, then write the code to satisfy the test. This is non-negotiable.

1. **Red Phase**: Write failing tests first before implementing any functionality
2. **Green Phase**: Write minimal code to make tests pass
3. **Refactor Phase**: Improve code while keeping tests green

### TDD Workflow
- **ALWAYS** write tests before implementing new features or fixing bugs
- When addressing a bug, first write a test that reproduces the bug (it should fail)
- When adding a feature, first write tests that define the expected behavior
- Start with the simplest failing test case
- Write only enough code to make the test pass
- Refactor with confidence knowing tests will catch regressions
- Run `mix test` frequently during development
- Use `mix test.watch` for continuous feedback

### Testing Patterns

Tests use comprehensive mocking to prevent external connections:
- APRS-IS connections are mocked in test environment
- Database uses sandbox mode for isolation
- External API calls mocked with Mox library
- Write unit tests for business logic, integration tests for workflows
- Test edge cases and error conditions thoroughly
- Maintain high test coverage with `mix test --cover`

## Code Style Guidelines

- **CRITICAL**: Never write production code without tests first
- Use LiveView for UI interactions, minimize JavaScript
- Prefer pattern matching over if/case statements
- Follow idiomatic Elixir conventions
- **CRITICAL**: ALWAYS run `mix format` BEFORE committing - this is non-negotiable
- Address any compiler warnings
- Run `mix dialyzer` and fix all errors/warnings
- **MANDATORY**: Run `mix compile --warnings-as-errors` and ensure it passes before considering any task complete
- Use function composition over nested conditionals
- Write descriptive test names that explain behavior

### Pre-Commit Checklist
**CRITICAL**: NEVER commit or push code with syntax errors or compilation failures. Always validate before committing.

**MANDATORY PRE-COMMIT STEPS** - Must be executed in this exact order before EVERY git commit:

1. **`mix format`** - ALWAYS run this FIRST before any git operations
   - This formats all Elixir code according to project standards
   - **NEVER skip this step** - unformatted code should never be committed
   
2. **`mix compile --warnings-as-errors`** - ensure no warnings or compilation errors
   - Must pass with zero warnings and zero errors
   
3. **`mix test`** - ensure all tests pass (at minimum, ensure no syntax/compilation errors)
   - At minimum verify no compilation failures even if some tests fail
   
4. **MANDATORY**: If any step fails, fix the issues before proceeding
   
5. Only after ALL checks pass should you commit and push your changes

**NEVER PUSH BROKEN CODE**: Syntax errors, compilation failures, or basic test failures should be fixed immediately before any git operations. Pushing broken code breaks CI/CD pipelines and wastes deployment resources.

**REMEMBER**: Always run `mix format` before every commit - this is non-negotiable and must become automatic habit.

## Git Commit Messages

When creating git commits:
- Write clear, concise commit messages following conventional commit format (e.g., `feat:`, `fix:`, `refactor:`, `docs:`)
- Focus on the "why" rather than the "what" in commit messages
- **DO NOT** add "Generated with Claude Code" or similar attribution to commit messages
- **DO NOT** add Co-Authored-By lines for Claude
- Keep commit messages professional and focused solely on the code changes

## Important Documentation Updates

- **MANDATORY**: Whenever you implement improvements or changes to the system:
  1. Update `/CHANGELOG.md` with:
     - Add new entries under `[Unreleased]` section
     - Use categories: Added, Changed, Fixed, Removed
     - Be specific and user-focused in descriptions
  2. Update `/docs/improvement-todos.md` with:
     - Mark completed items as done with the implementation date
     - Add any new improvements discovered during implementation
  - Update priority levels based on new insights
  - Document any technical decisions or trade-offs made
- This ensures continuity across sessions and helps track progress on system improvements

## Web Testing

- **MANDATORY**: When viewing any website or web application, always use Puppeteer to take screenshots and interact with the page
- Use `mcp__puppeteer__puppeteer_navigate`, `mcp__puppeteer__puppeteer_screenshot`, and other Puppeteer tools
- This ensures accurate visual feedback and proper testing of the user interface

## Deployment

The application supports Kubernetes deployment with manifests in `k8s/` directory and GitHub Actions CI/CD pipeline. Database migrations run automatically via init containers.

### Infrastructure

The application runs on a highly available infrastructure:
- **Kubernetes**: k3s cluster deployed across 3 VMs
- **Virtualization**: Proxmox VE hosting the VMs
- **Hardware**: 3 Intel N100 nodes, each with:
  - 32GB RAM
  - 1TB SSD storage
  - Low power consumption (~15W per node)
- **Distribution**: VMs spread across physical nodes for hardware redundancy

### Kubernetes Commands

The app is deployed in a k3s cluster with the following structure:
- **App name**: `aprs`
- **Namespace**: `aprs`
- **Deployment**: StatefulSet with 2-3 replicas
- **Manifests**: Located in `~/dev/infra/clusters/aprs/`

Common kubectl commands for debugging:
```bash
# Check pod status
kubectl get pods -n aprs

# Get logs from the app
kubectl logs -f deployment/aprs -n aprs

# Get logs from a specific pod
kubectl logs <pod-name> -n aprs

# Describe pod for events and details
kubectl describe pod <pod-name> -n aprs

# Restart the statefulset
kubectl rollout restart statefulset/aprs -n aprs

# Check statefulset status
kubectl rollout status statefulset/aprs -n aprs

# Execute commands in the pod (StatefulSet)
kubectl exec -it aprs-0 -n aprs -- /app/bin/aprsme remote

# Check cluster membership
kubectl exec -it <pod-name> -n aprs -- /app/bin/aprsme eval "Node.list()"

# Check leader status
kubectl exec -it <pod-name> -n aprs -- /app/bin/aprsme eval "Aprsme.Cluster.LeaderElection.is_leader?()"
```

### Clustering Architecture

The application uses distributed Erlang clustering to ensure only one APRS-IS connection across multiple replicas:

1. **StatefulSet Deployment**: 
   - Uses Kubernetes StatefulSet for stable pod names (aprs-0, aprs-1, etc.)
   - Headless service provides DNS entries for each pod
   - Stable network identities enable Erlang distribution

2. **Leader Election**: Uses `:global` registry for distributed leader election
   - Only the elected leader maintains the APRS-IS connection
   - Automatic failover when leader goes down
   - Leader election managed by `Aprsme.Cluster.LeaderElection`

3. **Connection Management**: 
   - `Aprsme.Cluster.ConnectionManager` starts/stops APRS-IS based on leadership
   - Uses `DynamicSupervisor` to manage connection lifecycle
   - Prevents duplicate connections and packet processing

4. **Cluster Configuration**:
   - Uses `libcluster` with Kubernetes.DNS strategy
   - Automatic node discovery via headless service
   - Erlang cookie configured via RELEASE_COOKIE environment variable
   - Environment variables:
     - `CLUSTER_ENABLED=true` - Enables clustering
     - `RELEASE_NODE` - Erlang node name
     - `RELEASE_COOKIE` - Erlang distribution cookie

4. **Deployment**:
   - Default replicas: 3 (configurable in `aprs-deployment.yaml`)
   - Only leader processes APRS packets
   - All nodes serve web traffic

## 1Password Integration

When working with passwords and secrets:
- **ALWAYS** use 1Password account ID: `YOOATCZZSVGH7AD6VABUVPORLI`
- Store all passwords, API keys, and secrets in 1Password
- Use the `op` CLI tool for programmatic access
- Never hardcode passwords in configuration files
