library(shiny)
library(DT)
library(DBI)
library(RSQLite)

DB_FILE <- "gacha_logger.db"

# -------------------------
# DB helpers
# -------------------------
conn_db <- function() dbConnect(SQLite(), DB_FILE)

init_db <- function() {
  conn <- conn_db()
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

compute_pity <- function(conn) {
  pulls <- dbGetQuery(conn, "SELECT rarity FROM pulls ORDER BY id")
  if (nrow(pulls) == 0) return(0)
  last5 <- tail(which(pulls$rarity == "5-Star"), 1)
  if (length(last5) == 0) return(nrow(pulls))
  nrow(pulls) - last5
}

recalculate_all_pity <- function(conn) {
  pulls <- dbGetQuery(conn, "SELECT id, rarity FROM pulls ORDER BY id")
  pity <- 0
  for (i in seq_len(nrow(pulls))) {
    if (pulls$rarity[i] == "5-Star") {
      pity <- 0
    } else {
      pity <- pity + 1
    }
    dbExecute(conn, "UPDATE pulls SET pity = ? WHERE id = ?", list(pity, pulls$id[i]))
  }
}

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

    tags$script(HTML("
      $(document).on('click', '.btn-custom', function() {
        $(this).blur();
      });
      $(function() {
        $('.nav-tab').on('click', function() {
          $('.nav-tab').removeClass('nav-tab-active');
          $(this).addClass('nav-tab-active');
          if ($(this).text().trim() === 'Analytics') {
            $('.input-card').hide();
            $('.table-card').hide();
            $('#analytics-panel').show();
          } else {
            $('.input-card').show();
            $('.table-card').show();
            $('#analytics-panel').hide();
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

      div(class = "input-card-wrapper",
        # Navigation Tabs
        div(class = "nav-tabs",
          div(class = "nav-tab nav-tab-active", "Logger"),
          div(class = "nav-tab", "Analytics")
        ),

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
              choices = c("Character", "Weapon", "Standard"),
              selected = "Character"
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
                    choices = c("All", "Character", "Weapon", "Standard"),
                    selected = "All"
                  )
              )
          ),
          
          DTOutput("table")
      ),

      # Analytics Panel (hidden by default)
      div(
        id = "analytics-panel",
        style = "display:none;",
        class = "analytics-content",
        # Left column
        div(class = "analytics-left",
          # Overall Statistics Card
          div(class = "stats-card overall-stats",
            div(class = "stats-card-header", "OVERALL STATISTICS"),
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
            div(class = "stats-card-header", HTML("&#10022;&#10022; RECENT PULLS")),
            uiOutput("recent_pulls_list")
          )
        ),
        # Right column
        div(class = "analytics-right",
          # Current Progress Card
          div(class = "stats-card current-progress",
            div(class = "progress-header",
              div(class = "progress-title",
                HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none"><path d="M3 3V21H21" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M7 14L12 9L16 13L21 8" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>'),
                span("CURRENT PROGRESS")
              ),
              selectInput(
                inputId = "progress_banner",
                label = NULL,
                choices = c("Character Event Wish", "Weapon Event Wish", "Standard Wish"),
                selected = "Character Event Wish",
                width = "180px"
              )
            ),
            div(class = "pity-info",
              div(class = "pity-label",
                span("5-star in "),
                textOutput("pity_pulls_ago", inline = TRUE),
                span(" pulls")
              ),
              div(class = "pity-counter",
                span(class = "pity-current", textOutput("current_pity", inline = TRUE)),
                span(class = "pity-max", "/ 90")
              )
            ),
            div(class = "pity-bar-container",
              div(class = "pity-bar", uiOutput("pity_bar_fill")),
              div(class = "pity-markers",
                span(class = "marker", "0"),
                span(class = "marker soft-pity", "Soft Pity (74)"),
                span(class = "marker hard-pity", "Hard Pity (90)")
              )
            )
          ),
          # Pull Trends Card
          div(class = "stats-card pull-trends",
            div(class = "stats-card-header",
              HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none"><path d="M3 3V21H21" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M7 14L12 9L16 13L21 8" stroke="#161A3E" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>'),
              span("PULL TRENDS")
            ),
            div(class = "trends-placeholder", "Pull trends chart coming soon")
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
    if (!is.null(banner_filter) && banner_filter != "All") {
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
        pageLength = 3,
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
    
    pity <- compute_pity(conn) + 1
    if (input$rarity == "5-Star") pity <- 0
    
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
    
    dbExecute(
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
    dbExecute(
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
    
    dbExecute(
      conn,
      "DELETE FROM pulls WHERE id = ?",
      row_id
    )

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
    updateSelectInput(session, "banner", selected = "Character")
    
    updateDateInput(
      session,
      "date",
      value = Sys.Date()
    )
  }
}

shinyApp(ui, server)

# For best user experience passte this command into the terminal first before
# running the app

# > options(shiny.launch.browser = TRUE)
