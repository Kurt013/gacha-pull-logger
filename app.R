library(shiny)
library(shinytoastr)
library(DT)
library(DBI)
library(RSQLite)

DB_FILE <- "gacha_logger.db"

# -------------------------
# DB helpers
# -------------------------
conn_db <- function() dbConnect(SQLite(), DB_FILE)

init_db <- function() {
  conn <- dbConnect(SQLite(), DB_FILE)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS banners (
      banner_id INTEGER PRIMARY KEY AUTOINCREMENT,
      banner_name TEXT UNIQUE
    )
  ")
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS pulls (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      banner_id INTEGER,
      type TEXT,
      name TEXT,
      rarity TEXT,
      pull_date TEXT,
      pity INTEGER
    )
  ")
  dbDisconnect(conn)
}
init_db()

compute_pity <- function(conn, banner_name) {
  pulls <- dbGetQuery(conn, "
    SELECT rarity FROM pulls
    JOIN banners ON pulls.banner_id = banners.banner_id
    WHERE banners.banner_name = ?
    ORDER BY pulls.id
  ", params = list(banner_name))
  if (nrow(pulls) == 0) return(0)
  last5 <- tail(which(pulls$rarity == "5-Star"), 1)
  if (length(last5) == 0) return(nrow(pulls))
  nrow(pulls) - last5
}

recalculate_all_pity <- function(conn) {
  banners <- dbGetQuery(conn, "SELECT banner_name FROM banners")
  for (banner in banners$banner_name) {
    pulls <- dbGetQuery(conn, "
      SELECT pulls.id, pulls.rarity FROM pulls
      JOIN banners ON pulls.banner_id = banners.banner_id
      WHERE banners.banner_name = ?
      ORDER BY pulls.id
    ", params = list(banner))
    pity <- 1
    for (i in seq_len(nrow(pulls))) {
      dbExecute(conn, "UPDATE pulls SET pity = ? WHERE id = ?", list(pity, pulls$id[i]))
      if (pulls$rarity[i] == "5-Star") {
        pity <- 1
      } else {
        pity <- pity + 1
      }
    }
  }
}

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  shinytoastr::useToastr(),

  tags$head(
    tags$title("Gacha Pull Logger"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

    tags$script(HTML("
      $(document).on('click', '.btn-custom', function() {
        $(this).blur();
      });
    ")),
    tags$script(HTML("
      $(function() {
        $(document).on('click', '.nav-tab', function() {
          // Remove active class from all tabs
          $('.nav-tab').removeClass('nav-tab-active');
          // Add active class to the clicked tab
          $(this).addClass('nav-tab-active');
          // Panel switching
          var tab = $(this).data('value');
          if(tab === 'logger') {
            $('#logger-panel').removeClass('inactive').addClass('active');
            $('#analytics-panel').removeClass('active').addClass('inactive');
          } else {
            $('#logger-panel').removeClass('active').addClass('inactive');
            $('#analytics-panel').removeClass('inactive').addClass('active');
          }
        });
      });
    "))
  ),
  
    div(class ="container",   
      # Title
      div(class = "header-title",
          "Gacha Pull", br(),
          span(class = "subtitle", 
                "Logger"
          )
          
      ),

      # Input Fields Card

      div(class = "panel-wrapper",
        # Navigation Tabs
        div(class = "nav-tabs",
          div(
            class = "nav-tab nav-tab-active",
            `data-value` = "logger",
            onclick = "Shiny.setInputValue('tab', 'logger', {priority: 'event'})",
            "Logger"
          ),
          div(
            class = "nav-tab",
            `data-value` = "analytics",
            onclick = "Shiny.setInputValue('tab', 'analytics', {priority: 'event'})",
            "Analytics"
          )
        ),

        div(id = "logger-panel", class="active",
          div(class= "input-card",
            div(class = "col col-1",
              selectInput(
                inputId = "type",
                label   = "Type",
                choices = c("Weapon", "Character"),
                selected = "Weapon"
              ),
              textInput(
                inputId = "name",
                label   = "Name"
              )
            ),

            div(class = "col col-2",
              selectInput(
                inputId = "rarity",
                label   = "Rarity",
                choices = c("3-Star", "4-Star", "5-Star"),
                selected = "3-Star"
              )
            ),
            
            div(class = "col col-3",
              selectInput(
                inputId = "banner",
                label   = "Banner",
                choices = c("Character Event Wish", "Weapon Event Wish", "Standard Event Wish"),
                selected = "Character Event Wish"
              ),
              div(class = "date-container",
                HTML('
                <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16" fill="none">
                  <path d="M10.6667 1.33333V3.99999M5.33333 1.33333V3.99999M2 6.66666H14M3.33333 2.66666H12.6667C13.403 2.66666 14 3.26362 14 3.99999V13.3333C14 14.0697 13.403 14.6667 12.6667 14.6667H3.33333C2.59695 14.6667 2 14.0697 2 13.3333V3.99999C2 3.26362 2.59695 2.66666 3.33333 2.66666Z" stroke="#1E1E1E" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
                '),
                dateInput(
                  inputId = "date",
                  label = "Pull Date",
                  value = Sys.Date(),
                  format = "yyyy-mm-dd"
                )
              )
            ),

            div(class = "col col-4", 
              div(
                class = "btn-row",
                actionButton(
                  "add",
                  label = tagList(
                    HTML('
                      <svg xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="0 0 40 40" fill="none">
                        <path d="M20 8.33334V31.6667M8.33334 20H31.6667" stroke="currentColor" stroke-width="3.5" stroke-linecap="round" stroke-linejoin="round"/>
                      </svg>
                    '),
                    "Add"
                  ),
                  class = "btn-custom btn-add"
                ),
                
                actionButton(
                  "update",
                  label = tagList(
                    HTML('
                      <svg xmlns="http://www.w3.org/2000/svg" width="28" height="28" viewBox="0 0 28 28" fill="none">
                        <g clip-path="url(#clip0_17_58)">
                          <path d="M26.8333 4.66667V11.6667M26.8333 11.6667H19.8333M26.8333 11.6667L21.42 6.58C20.1661 5.3255 18.6149 4.40908 16.911 3.91624C15.2072 3.4234 13.4063 3.37021 11.6763 3.76164C9.94637 4.15306 8.34376 4.97634 7.01803 6.15466C5.69231 7.33297 4.68669 8.82792 4.09499 10.5M1.16666 23.3333V16.3333M1.16666 16.3333H8.16666M1.16666 16.3333L6.57999 21.42C7.83386 22.6745 9.38509 23.5909 11.0889 24.0838C12.7928 24.5766 14.5937 24.6298 16.3237 24.2384C18.0536 23.8469 19.6562 23.0237 20.9819 21.8453C22.3077 20.667 23.3133 19.1721 23.905 17.5" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round"/>
                        </g>
                        <defs>
                          <clipPath id="clip0_17_58">
                            <rect width="28" height="28" fill="currentColor"/>
                          </clipPath>
                        </defs>
                      </svg>
                    '),
                    "Update"
                  ),
                  class = "btn-custom btn-update"
                ),
                
                actionButton(
                  "delete",
                  label = tagList(
                    HTML('
                      <svg xmlns="http://www.w3.org/2000/svg" width="35" height="35" viewBox="0 0 35 35" fill="none">
                        <path d="M7.29167 29.1667C7.29167 29.9402 7.59896 30.6821 8.14594 31.2291C8.69292 31.776 9.43479 32.0833 10.2083 32.0833H24.7917C25.5652 32.0833 26.3071 31.776 26.8541 31.2291C27.401 30.6821 27.7083 29.9402 27.7083 29.1667V11.6667H30.625V8.75H24.7917V5.83334C24.7917 5.05979 24.4844 4.31792 23.9374 3.77094C23.3904 3.22396 22.6485 2.91667 21.875 2.91667H13.125C12.3515 2.91667 11.6096 3.22396 11.0626 3.77094C10.5156 4.31792 10.2083 5.05979 10.2083 5.83334V8.75H4.375V11.6667H7.29167V29.1667ZM13.125 5.83334H21.875V8.75H13.125V5.83334ZM11.6667 11.6667H24.7917V29.1667H10.2083V11.6667H11.6667Z" fill="currentColor"/>
                        <path d="M13.125 14.5833H16.0417V26.25H13.125V14.5833ZM18.9583 14.5833H21.875V26.25H18.9583V14.5833Z" fill="currentColor"/>
                      </svg>
                    '),
                    "Delete"
                  ),
                  class = "btn-custom btn-delete"
                ),
                
                actionButton(
                  "clear",
                  label = tagList(
                    HTML('
                      <svg xmlns="http://www.w3.org/2000/svg" width="36" height="36" viewBox="0 0 36 36" fill="none">
                        <path d="M18.8681 2.9339L11.3769 10.4965L3.91722 18.2024C3.32975 18.799 3 19.606 3 20.4473C3 21.2885 3.32975 22.0955 3.91722 22.6922L10.6988 29.5383C10.9925 29.8324 11.389 29.9982 11.8027 30H30.7279V26.8158H19.6882L31.0749 15.3206C31.3682 15.0249 31.6008 14.6737 31.7596 14.2871C31.9183 13.9006 32 13.4862 32 13.0678C32 12.6493 31.9183 12.235 31.7596 11.8484C31.6008 11.4618 31.3682 11.1106 31.0749 10.8149L23.3313 2.9339C23.0384 2.63784 22.6905 2.40297 22.3076 2.24273C21.9247 2.08248 21.5143 2 21.0997 2C20.6852 2 20.2748 2.08248 19.8919 2.24273C19.509 2.40297 19.1611 2.63784 18.8681 2.9339ZM12.4493 26.8158L6.14093 20.4473L13.6322 12.7414L14.7992 11.5473L22.6059 19.4283L15.4143 26.6884L15.3039 26.8158H12.4493Z" fill="currentColor"/>
                      </svg>
                    '),
                    "Clear"
                  ),
                  class = "btn-custom btn-clear"
                )
              )
            )
          ),

          # Table Card
          div(class = "table-card",
              div(class = "table-card-header",
                  div(class = "left-side", 
                      tagList(
                        HTML('
                          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none">
                            <path d="M21 8C20.798 8 16.15 8.029 12 10.008C7.85 8.029 3.202 8 3 8C2.73478 8 2.48043 8.10536 2.29289 8.29289C2.10536 8.48043 2 8.73478 2 9V18.883C2 19.0172 2.02701 19.15 2.0794 19.2735C2.1318 19.397 2.20852 19.5087 2.305 19.602C2.5 19.79 2.785 19.907 3.034 19.882L3.161 19.881C3.844 19.881 7.457 19.979 11.577 21.906C11.593 21.914 11.611 21.911 11.627 21.917C11.746 21.966 11.871 22 12 22C12.129 22 12.254 21.966 12.374 21.917C12.39 21.911 12.408 21.914 12.424 21.906C16.544 19.978 20.157 19.881 20.84 19.881L20.967 19.882C21.205 19.907 21.5 19.79 21.696 19.602C21.89 19.413 22 19.153 22 18.883V9C22 8.73478 21.8946 8.48043 21.7071 8.29289C21.5196 8.10536 21.2652 8 21 8ZM4 10.049C5.485 10.16 8.381 10.529 11 11.741V19.483C8 18.308 5.41 17.989 4 17.907V10.049ZM20 17.907C18.59 17.989 16 18.308 13 19.483V11.741C15.619 10.529 18.515 10.16 20 10.049V17.907Z" fill="#222648"/>
                            <path d="M12 8C13.6569 8 15 6.65685 15 5C15 3.34315 13.6569 2 12 2C10.3431 2 9 3.34315 9 5C9 6.65685 10.3431 8 12 8Z" fill="#222648"/>
                          </svg>
                        '),
                        div("Pull Records "),
                        span(class = "subheader", "(Click a row to select)")
                      )         
                  ),
                  div(class = "right-side inline-field",
                      selectInput(
                        inputId = "filter",
                        label   = "Filter By: ",
                        choices = c("All (Banner)", "Character Event Wish", "Weapon Event Wish", "Standard Event Wish"),
                        selected = "All (Banner)"
                      )
                  )
              ),
              DTOutput("table")
          )
        ),
        
        # Analytics Panel (hidden by default)
        div(
          id = "analytics-panel",
          class = "analytics-content inactive",
          # Left column
          div(class = "analytics-left",
            # Overall Statistics Card
            div(class = "stats-card overall-stats",
              div(class = "stats-card-title", "Overall Statistics"),
              div(class = "stats-list",
                div(class = "stat-row",
                  span(class = "stat-label", "Total Pulls:"),
                  span(class = "stat-value", textOutput("total_pulls", inline = TRUE))
                ),
                div(class = "stat-row",
                  span(class = "stat-label", "Primogems Spent:"),
                  span(class = "stat-value", textOutput("primogems_spent", inline = TRUE))
                ),
                div(class = "stat-row",
                  span(class = "stat-label", "Luck Index:"),
                  span(class = "stat-value", textOutput("luck_index", inline = TRUE))
                ),
                div(class = "stat-row",
                  span(class = "stat-label", "Average Pity:"),
                  span(class = "stat-value", textOutput("avg_pity", inline = TRUE))
                )
              )
            ),
            # Recent Pulls Card
            div(class = "stats-card recent-pulls-card",
              div(class = "stats-card-header",
                HTML('<svg xmlns="http://www.w3.org/2000/svg" width="19" height="15" viewBox="0 0 19 15" fill="none"><path d="M12.3929 3.5C12.3929 5.433 10.6591 7 8.5 7C6.34093 7 4.60715 5.433 4.60715 3.5C4.60715 1.567 6.34093 0 8.5 0C10.6591 0 12.3929 1.567 12.3929 3.5Z" fill="#222648"/><path d="M17 4.5C16.798 4.5 12.15 4.529 8 6.508C3.85 4.529 -0.798 4.5 -1 4.5C-1.26522 4.5 -1.51957 4.60536 -1.70711 4.79289C-1.89464 4.98043 -2 5.23478 -2 5.5V15.383C-2 15.5172 -1.97299 15.65 -1.9206 15.7735C-1.8682 15.897 -1.79148 16.0087 -1.695 16.102C-1.5 16.29 -1.215 16.407 -0.966 16.382L-0.839 16.381C-0.156 16.381 3.457 16.479 7.577 18.406C7.593 18.414 7.611 18.411 7.627 18.417C7.746 18.466 7.871 18.5 8 18.5C8.129 18.5 8.254 18.466 8.374 18.417C8.39 18.411 8.408 18.414 8.424 18.406C12.544 16.478 16.157 16.381 16.84 16.381L16.967 16.382C17.205 16.407 17.5 16.29 17.696 16.102C17.89 15.913 18 15.653 18 15.383V5.5C18 5.23478 17.8946 4.98043 17.7071 4.79289C17.5196 4.60536 17.2652 4.5 17 4.5ZM0 6.549C1.485 6.66 4.381 7.029 7 8.241V15.983C4 14.808 1.41 14.489 0 14.407V6.549ZM16 14.407C14.59 14.489 12 14.808 9 15.983V8.241C11.619 7.029 14.515 6.66 16 6.549V14.407Z" fill="#222648"/></svg>'),
                span("Recent Pulls")
              ),
              uiOutput("recent_pulls_list")
            )
          ),
          # Right column
          div(class = "analytics-right",
            # Current Progress Card
            div(class = "stats-card current-progress",
              div(class = "progress-header",
                div(class = "progress-title",
                  HTML('<svg xmlns="http://www.w3.org/2000/svg" width="25" height="24" viewBox="0 0 25 24" fill="none"><path d="M20.0312 18V6.5" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M12.5156 18V3" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M5 18V10.5" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>'),
                  span("Current Progress")
                ),
                selectInput(
                  inputId = "progress_banner",
                  label = NULL,
                  choices = c("Character Event Wish", "Weapon Event Wish", "Standard Event Wish"),
                  selected = "Character Event Wish",
                  width = "220px"
                )
              ),
              div(class = "pity-progress-box",
                div(class = "pity-info",
                  div(class = "pity-label",
                    span("5-star in "),
                    span(class = "pity-highlight", textOutput("pity_pulls_ago", inline = TRUE)),
                    span(" pulls")
                  ),
                  div(class = "pity-counter",
                    span(class = "pity-current", textOutput("current_pity", inline = TRUE)),
                    span(class = "pity-max", " / 90")
                  )
                ),
                div(class = "pity-bar-container",
                  div(class = "pity-bar"),
                  div(class = "pity-markers",
                    span(class = "marker", "0"),
                    span(class = "marker soft-pity", "Soft Pity (74)"),
                    span(class = "marker hard-pity", "Hard Pity (90)")
                  )
                )
              )
            ),
            # Pull Trends Card
            div(class = "stats-card pull-trends",
              div(class = "stats-card-header",
                HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none"><path d="M5 3V19H21" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M19 5L13 11L9 7L5 11" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>'),
                span("Pull Trends")
              ),
              div(class = "trends-chart-area",
                div(class = "trends-placeholder", "Pull trends chart coming soon")
              )
            )
          )
        )
      )
    )
  )

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  # ---- Reactive data holder ----
  data <- reactiveVal(data.frame())



  # ---- Customize Data table ----
  output$table <- renderDT({
    
    df <- data()

    # Apply banner filter if not "All"
    banner_filter <- input$filter
    if (!is.null(banner_filter) && banner_filter != "All (Banner)") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }
    
    colnames(df) <- c(
      "ID",
      "Type",
      "Name",
      "Rarity",
      "Banner",
      "Date",
      "Pity"
    )
    
    datatable(
      df,
      rownames = FALSE,
      selection = "single",
      options = list(
        scrollCollapse = TRUE,
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "130px",
        searching = FALSE,   
        lengthChange = FALSE
      )
    )
  })

  # Pre-fill form when a row is selected
  observeEvent(input$table_rows_selected, {
    df <- data()
    selected_row <- input$table_rows_selected
    if (length(selected_row) == 0) return()
    row <- df[selected_row, ]
    
    updateSelectInput(session, "type", selected = row$type)
    updateTextInput(session, "name", value = row$name)
    updateSelectInput(session, "rarity", selected = row$rarity)
    updateSelectInput(session, "banner", selected = row$banner)
    updateDateInput(session, "date", value = as.Date(row$pull_date))
  })
  
  # ----- CRUD FUNCTIONALITY -----
  # ---- ADD (Create) ----
  observeEvent(input$add, {
    
    req(nzchar(input$name))
    
    conn <- conn_db()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    pity <- compute_pity(conn, input$banner)
    
    dbExecute(
      conn,
      "INSERT OR IGNORE INTO banners (banner_name) VALUES (?)",
      input$banner
    )
    
    banner_id <- dbGetQuery(
      conn,
      "SELECT banner_id FROM banners WHERE banner_name = ?",
      input$banner
    )$banner_id
    
    result <- dbExecute(
      conn,
      "INSERT INTO pulls (banner_id, type, name, rarity, pull_date, pity)
       VALUES (?, ?, ?, ?, ?, ?)",
      list(
        banner_id,
        input$type,
        input$name,
        input$rarity,
        as.character(input$date),
        pity
      )
    )
  
    if (result == 1) {
      # Success
      shinytoastr::toastr_success("Pull added successfully!", progressBar = TRUE, showMethod = "slideDown")
    } else {
      # Failure
      shinytoastr::toastr_error("Failed to add pull.", progressBar = TRUE, showMethod = "slideDown")
    }

    clear_fields()
    recalculate_all_pity(conn)
    refresh()
  })

  # ---- READ ----
  refresh <- function() {
    conn <- conn_db()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    df <- dbGetQuery(conn, "
      SELECT pulls.id, pulls.type, pulls.name, pulls.rarity,
             banners.banner_name AS banner,
             pulls.pull_date, pulls.pity
      FROM pulls
      LEFT JOIN banners ON pulls.banner_id = banners.banner_id
      ORDER BY pulls.id DESC
    ")
    
    data(df)
  }

  refresh()
  
  # ---- UPDATE ----
  observeEvent(input$update, {
    req(input$table_rows_selected)
    req(nzchar(input$name))
    
    conn <- conn_db()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    # Get selected row's ID
    df <- data()
    selected_row <- input$table_rows_selected
    if (length(selected_row) == 0) return()
    row_id <- df$id[selected_row]
    
    # Update banner if needed
    dbExecute(
      conn,
      "INSERT OR IGNORE INTO banners (banner_name) VALUES (?)",
      input$banner
    )
    banner_id <- dbGetQuery(
      conn,
      "SELECT banner_id FROM banners WHERE banner_name = ?",
      input$banner
    )$banner_id
    
    # Update the pull
    result <- dbExecute(
      conn,
      "UPDATE pulls SET banner_id = ?, type = ?, name = ?, rarity = ?, pity = ?
      WHERE id = ?",
      list(
        banner_id,
        input$type,
        input$name,
        input$rarity,
        as.character(input$date),
        row_id
      )
    )

    if (result == 1) {
      # Success
      shinytoastr::toastr_success("Pull updated successfully!", progressBar = TRUE, showMethod = "slideDown")
    } else {
      # Failure
      shinytoastr::toastr_error("Failed to update pull.", progressBar = TRUE, showMethod = "slideDown")
    }
    
    clear_fields()
    recalculate_all_pity(conn)
    refresh()
  })

  # ---- DELETE ----
  observeEvent(input$delete, {
    req(input$table_rows_selected)
    
    conn <- conn_db()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    df <- data()
    selected_row <- input$table_rows_selected
    if (length(selected_row) == 0) return()
    row_id <- df$id[selected_row]
    
    result <- dbExecute(
      conn,
      "DELETE FROM pulls WHERE id = ?",
      row_id
    )

    if (result == 1) {
      # Success
      shinytoastr::toastr_success("Pull deleted successfully!", progressBar = TRUE, showMethod = "slideDown")
    } else {
      # Failure
      shinytoastr::toastr_error("Failed to delete pull.", progressBar = TRUE, showMethod = "slideDown")
    }

    clear_fields()
    recalculate_all_pity(conn)
    refresh()
  })

  # ---- CLEAR (Reset form) ----
  observeEvent(input$clear, {
    clear_fields()
  })

  clear_fields <- function() {
    updateTextInput(session, "name", value = "")
    updateSelectInput(session, "type", selected = "Weapon")
    updateSelectInput(session, "rarity", selected = "3-Star")
    updateSelectInput(session, "banner", selected = "Character Event Wish")
    
    updateDateInput(
      session,
      "date",
      value = Sys.Date()
    )
  }

  
  # ---- ANALYTICS OUTPUTS ----
  # Total Pulls
  output$total_pulls <- renderText({
    df <- data()

    pulls <- if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      0
    } else {
      nrow(df)
    }

    as.character(pulls)
  })

  # Primogems Spent (160 per pull)
  output$primogems_spent <- renderText({
    df <- data()

    if (is.null(df) || nrow(df) == 0) {
      return("0")
    }

    format(nrow(df) * 160, big.mark = ",")
  })

  # Luck Index (5-star rate)
  output$luck_index <- renderText({
    df <- data()

    if (is.null(df) || nrow(df) == 0) {
      return("0%")
    }

    five_star_count <- sum(df$rarity == "5-Star")
    sprintf("%.2f%%", 100 * five_star_count / nrow(df))
  })

  # Average Pity
  output$avg_pity <- renderText({
    df <- data()
    five_stars <- df[df$rarity == "5-Star", ]
    if (nrow(five_stars) == 0) return("N/A")
    round(mean(five_stars$pity, na.rm = TRUE))
  })

  # Current Pity (for selected banner)
  output$current_pity <- renderText({
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) return(0)
    banner_df <- df[df$banner == banner_filter, ]
    if (nrow(banner_df) == 0) return(0)
    # Find pulls since last 5-star
    last5_idx <- max(which(banner_df$rarity == "5-Star"), 0)
    if (last5_idx == 0) return(nrow(banner_df))
    nrow(banner_df) - last5_idx
  })

  # Pity pulls ago
  output$pity_pulls_ago <- renderText({
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) return(0)
    banner_df <- df[df$banner == banner_filter, ]
    if (nrow(banner_df) == 0) return(0)
    last5_idx <- max(which(banner_df$rarity == "5-Star"), 0)
    if (last5_idx == 0) return(nrow(banner_df))
    nrow(banner_df) - last5_idx
  })

  # Recent Pulls List (last 5 pulls with 4-star or 5-star)
  output$recent_pulls_list <- renderUI({
    df <- data()
    if (nrow(df) == 0) {
      return(div(class = "no-pulls", "No pulls recorded yet"))
    }
    # Filter to 4-star and 5-star, take last 5
    rare_pulls <- df[df$rarity %in% c("4-Star", "5-Star"), ]
    rare_pulls <- head(rare_pulls, 5)
    if (nrow(rare_pulls) == 0) {
      return(div(class = "no-pulls", "No rare pulls recorded yet"))
    }
    pull_items <- lapply(seq_len(nrow(rare_pulls)), function(i) {
      row <- rare_pulls[i, ]
      star_class <- if (row$rarity == "5-Star") "stars-5" else "stars-4"
      border_color <- if (row$rarity == "5-Star") "gold" else "purple"
      stars <- if (row$rarity == "5-Star") {
        paste(rep("\u2605", 5), collapse = "")
      } else {
        paste(rep("\u2605", 4), collapse = "")
      }
      div(class = paste("recent-pull-item", border_color),
        span(class = paste("pull-stars", star_class), stars),
        span(class = "pull-name", row$name),
        span(class = paste("pull-pity", star_class), row$pity)
      )
    })
    do.call(tagList, pull_items)
  })
}

shinyApp(ui, server)