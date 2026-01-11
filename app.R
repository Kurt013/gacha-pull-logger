library(shiny)
library(shinyjs)
library(shinytoastr)
library(DT)
library(DBI)
library(RSQLite)
library(plotly)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)

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
  if (nrow(pulls) == 0) return(1)  # First pull starts at pity 1
  last5 <- tail(which(pulls$rarity == "5-Star"), 1)
  if (length(last5) == 0) return(nrow(pulls) + 1)  # No 5-star yet, next pity is count + 1
  nrow(pulls) - last5 + 1  # Pulls since last 5-star + 1
}

recalculate_all_pity <- function(conn) {
  banners <- dbGetQuery(conn, "SELECT banner_name FROM banners")
  
  # Start transaction for batch updates
  dbExecute(conn, "BEGIN TRANSACTION")
  
  tryCatch({
    for (banner in banners$banner_name) {
      pulls <- dbGetQuery(conn, "
        SELECT pulls.id, pulls.rarity FROM pulls
        JOIN banners ON pulls.banner_id = banners.banner_id
        WHERE banners.banner_name = ?
        ORDER BY pulls.id
      ", params = list(banner))
      
      if (nrow(pulls) == 0) next
      
      pity_values <- integer(nrow(pulls))
      pity <- 1
      for (i in seq_len(nrow(pulls))) {
        pity_values[i] <- pity
        if (pulls$rarity[i] == "5-Star") {
          pity <- 1
        } else {
          pity <- pity + 1
        }
      }
      
      chunk_size <- 500
      n_chunks <- ceiling(nrow(pulls) / chunk_size)
      
      for (chunk in seq_len(n_chunks)) {
        start_idx <- (chunk - 1) * chunk_size + 1
        end_idx <- min(chunk * chunk_size, nrow(pulls))
        chunk_ids <- pulls$id[start_idx:end_idx]
        chunk_pity <- pity_values[start_idx:end_idx]
        
        case_parts <- paste0("WHEN ", chunk_ids, " THEN ", chunk_pity, collapse = " ")
        sql <- sprintf(
          "UPDATE pulls SET pity = CASE id %s END WHERE id IN (%s)",
          case_parts,
          paste(chunk_ids, collapse = ",")
        )
        dbExecute(conn, sql)
      }
    }
    
    dbExecute(conn, "COMMIT")
  }, error = function(e) {
    dbExecute(conn, "ROLLBACK")
    stop(e)
  })
}

get_pity_thresholds <- function(banner) {
  if (banner == "Weapon Event Wish") {
    list(soft = 63, hard = 80)
  } else {
    list(soft = 74, hard = 90)
  }
}

# Null-coalescing operator helper (for R versions < 4.1)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x


# -------------------------
# UI
# -------------------------

loaderUI <- function() {
  div(class = "loader-overlay",
    div(class = "loader-container",
      div(class = "spinner", 
        div(class = "loader-decoration",
          HTML('
            <svg xmlns="http://www.w3.org/2000/svg" width="177" height="131" viewBox="0 0 177 131" fill="none">
              <path d="M88.4004 2.86328C88.5518 3.63841 88.7402 4.55663 88.9717 5.59375C89.729 8.98703 90.9369 13.659 92.7422 18.7734C96.3458 28.9826 102.356 41.0407 111.979 48.1631C121.578 55.2687 137.813 59.702 151.568 62.3623C158.461 63.6952 164.757 64.5873 169.33 65.1465C170.217 65.2549 171.039 65.3494 171.788 65.4336C171.039 65.5178 170.217 65.6132 169.33 65.7217C164.757 66.2809 158.461 67.1739 151.568 68.5068C137.813 71.1671 121.578 75.6004 111.979 82.7061C102.357 89.8285 96.3458 101.887 92.7422 112.096C90.937 117.21 89.729 121.881 88.9717 125.274C88.7403 126.311 88.5518 127.229 88.4004 128.004C88.249 127.229 88.0604 126.311 87.8291 125.274C87.0718 121.881 85.8637 117.21 84.0586 112.096C80.4549 101.886 74.4435 89.8285 64.8213 82.7061C55.2217 75.6004 38.9878 71.1671 25.2324 68.5068C18.3401 67.1739 12.0437 66.2809 7.4707 65.7217C6.58356 65.6132 5.76112 65.5178 5.01172 65.4336C5.76109 65.3494 6.5836 65.255 7.4707 65.1465C12.0437 64.5873 18.3402 63.6953 25.2324 62.3623C38.9876 59.7021 55.2217 55.2686 64.8213 48.1631C74.4435 41.0407 80.4549 28.9827 84.0586 18.7734C85.8639 13.6591 87.0718 8.98702 87.8291 5.59375C88.0606 4.55663 88.2489 3.63841 88.4004 2.86328Z" fill="#F5F1E6" stroke="currentColor"/>
              <path d="M88.3008 16.2457C88.3008 16.2457 91.893 41.089 105.885 51.4565C119.877 61.824 153.406 64.4856 153.406 64.4856C153.406 64.4856 119.877 67.1473 105.885 77.5148C91.893 87.8823 88.3008 112.726 88.3008 112.726C88.3008 112.726 84.7085 87.8823 70.7163 77.5148C56.7241 67.1473 23.1951 64.4856 23.1951 64.4856C23.1951 64.4856 56.7241 61.824 70.7163 51.4565C84.7085 41.089 88.3008 16.2457 88.3008 16.2457Z" fill="currentColor"/>
            </svg>
          ')
        ),
        div(class = "loader l1"),
        div(class = "loader l2")
      )
    )
  )
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinytoastr::useToastr(),

  tags$head(
    tags$title("Gacha Pull Logger"),
    tags$link(rel = "icon", type = "image/png", href = "assets/gacha-pull-logger-icon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

    tags$script(HTML("
      $(document).on('click', '.btn-custom', function() {
        $(this).blur();
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updatePityBar', function(message) {
        var pct = message.width;
        $('#pity-fill').css('width', pct + '%');
        // Scale gradient so it shows only the portion up to current pity
        // At pct%, we want to see 0-pct% of the full gradient
        if (pct > 0) {
          var bgSize = (100 / pct) * 100;
          $('#pity-fill').css('background-size', bgSize + '% 100%');
        }
      });
    ")),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateSoftPityMarker', function(message) {
        var pct = message.percent;
        $('.soft-pity-indicator').css('left', (pct - 0.17) + '%');
        $('.marker.soft-pity').css('flex-basis', (pct + 3.5) + '%');
      });
    ")),

    tags$script(HTML("
      // Toggle password visibility
      $(document).on('click', '.password-toggle', function() {
        var input = $('#login_password');
        var icon = $(this);
        if (input.attr('type') === 'password') {
          input.attr('type', 'text');
          icon.addClass('visible');
        } else {
          input.attr('type', 'password');
          icon.removeClass('visible');
        }
      });
    ")),
    tags$script(HTML("
      // Session persistence using localStorage
      $(document).on('shiny:connected', function() {
        var isLoggedIn = localStorage.getItem('gacha_logged_in');
        if (isLoggedIn === 'true') {
          setTimeout(function() {
            Shiny.setInputValue('restore_session', Date.now(), {priority: 'event'});
          }, 100);
        }
        // Trigger auth check after 500ms delay (only once)
        setTimeout(function() {
          Shiny.setInputValue('auth_ready', Date.now(), {priority: 'event'});
        }, 500);
      });
      
      // Handler to save login state
      Shiny.addCustomMessageHandler('saveLoginState', function(message) {
        if (message.logged_in) {
          localStorage.setItem('gacha_logged_in', 'true');
        } else {
          localStorage.removeItem('gacha_logged_in');
        }
      });
    ")),
    tags$script(HTML("
      // Handler for clicking recent pull items
      $(document).on('click', '.recent-pull-item', function() {
        var itemName = $(this).data('name');
        var itemType = $(this).data('type');
        var itemRarity = $(this).data('rarity');
        var itemDate = $(this).data('date');
        var itemPity = $(this).data('pity');
        Shiny.setInputValue('recent_pull_clicked', {
          name: itemName,
          type: itemType,
          rarity: itemRarity,
          date: itemDate,
          pity: itemPity,
          timestamp: Date.now()
        }, {priority: 'event'});
      });
    "))
  ),
  
  loaderUI(),

  shinyjs::hidden(
    div(id = "login_panel", class = "login-page",
      div(class = "blur-overlay"),
      div(class = "login-panel",
        img(class = "login-border", src = "assets/login_border.webp"),
        div(class = "login-content",
          div(class = "login-title",
            div(class = "welcome-text",
              span(class = "welcome-main", "Welcome"),
              span(class = "welcome-sub", "Traveler")
            ),
            div(class = "login-subtext-box",
              div(class = "login-decoration",
                HTML('
                  <svg xmlns="http://www.w3.org/2000/svg" width="177" height="131" viewBox="0 0 177 131" fill="none">
                    <path d="M88.4004 2.86328C88.5518 3.63841 88.7402 4.55663 88.9717 5.59375C89.729 8.98703 90.9369 13.659 92.7422 18.7734C96.3458 28.9826 102.356 41.0407 111.979 48.1631C121.578 55.2687 137.813 59.702 151.568 62.3623C158.461 63.6952 164.757 64.5873 169.33 65.1465C170.217 65.2549 171.039 65.3494 171.788 65.4336C171.039 65.5178 170.217 65.6132 169.33 65.7217C164.757 66.2809 158.461 67.1739 151.568 68.5068C137.813 71.1671 121.578 75.6004 111.979 82.7061C102.357 89.8285 96.3458 101.887 92.7422 112.096C90.937 117.21 89.729 121.881 88.9717 125.274C88.7403 126.311 88.5518 127.229 88.4004 128.004C88.249 127.229 88.0604 126.311 87.8291 125.274C87.0718 121.881 85.8637 117.21 84.0586 112.096C80.4549 101.886 74.4435 89.8285 64.8213 82.7061C55.2217 75.6004 38.9878 71.1671 25.2324 68.5068C18.3401 67.1739 12.0437 66.2809 7.4707 65.7217C6.58356 65.6132 5.76112 65.5178 5.01172 65.4336C5.76109 65.3494 6.5836 65.255 7.4707 65.1465C12.0437 64.5873 18.3402 63.6953 25.2324 62.3623C38.9876 59.7021 55.2217 55.2686 64.8213 48.1631C74.4435 41.0407 80.4549 28.9827 84.0586 18.7734C85.8639 13.6591 87.0718 8.98702 87.8291 5.59375C88.0606 4.55663 88.2489 3.63841 88.4004 2.86328Z" fill="#F5F1E6" stroke="#EEE6D9"/>
                    <path d="M88.3008 16.2457C88.3008 16.2457 91.893 41.089 105.885 51.4565C119.877 61.824 153.406 64.4856 153.406 64.4856C153.406 64.4856 119.877 67.1473 105.885 77.5148C91.893 87.8823 88.3008 112.726 88.3008 112.726C88.3008 112.726 84.7085 87.8823 70.7163 77.5148C56.7241 67.1473 23.1951 64.4856 23.1951 64.4856C23.1951 64.4856 56.7241 61.824 70.7163 51.4565C84.7085 41.089 88.3008 16.2457 88.3008 16.2457Z" fill="#EFE7DA"/>
                  </svg>
                ')
              ),
              p(class = "login-subtext", "Log in and turn your pulls into"),
              p(class = "login-subtext-highlight", "insights")
            )
          ),
          
          div(class = "login-fields",
            div(class = "login-input-group",
              tags$label(`for` = "login_username", "Username"),
              textInput("login_username", label = NULL, placeholder = "")
            ),
            div(class = "login-input-group password-group",
              tags$label(`for` = "login_password", "Password"),
              passwordInput("login_password", label = NULL, placeholder = ""),
              div(class = "password-toggle",
                HTML('
                  <svg class="icon-hidden" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none">
                    <path d="M8.073 12.194L4.212 8.33297C2.692 9.98997 2.116 11.65 2.106 11.684L2 12L2.105 12.316C2.127 12.383 4.421 19 12.054 19C12.983 19 13.829 18.898 14.606 18.727L11.86 15.981C10.8713 15.9325 9.93595 15.518 9.23598 14.818C8.53601 14.118 8.12147 13.1827 8.073 12.194ZM12.054 4.99997C10.199 4.99997 8.679 5.40397 7.412 5.99797L3.707 2.29297L2.293 3.70697L20.293 21.707L21.707 20.293L18.409 16.995C21.047 15.042 21.988 12.358 22.002 12.316L22.107 12L22.002 11.684C21.98 11.617 19.687 4.99997 12.054 4.99997ZM13.96 12.546C14.147 11.869 13.988 11.107 13.468 10.586C12.948 10.065 12.185 9.90697 11.508 10.094L10 8.58597C10.618 8.20595 11.3285 8.00322 12.054 7.99997C14.26 7.99997 16.054 9.79397 16.054 12C16.051 12.7253 15.8479 13.4357 15.467 14.053L13.96 12.546Z" fill="#161A3E"/>
                  </svg>
                  <svg class="icon-visible" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none">
                    <path d="M14 12C12.905 12 12 11.095 12 10C12 9.646 12.103 9.317 12.268 9.027C12.178 9.02 12.092 9 12 9C11.206 9.00524 10.4459 9.32299 9.88447 9.88447C9.32299 10.4459 9.00524 11.206 9 12C9 13.642 10.358 15 12 15C13.641 15 15 13.642 15 12C15 11.908 14.98 11.822 14.973 11.732C14.683 11.897 14.354 12 14 12Z" fill="url(#paint0_linear_181_334)"/>
                    <path d="M12 5C4.36701 5 2.07301 11.617 2.05201 11.684L1.94601 12L2.05101 12.316C2.07301 12.383 4.36701 19 12 19C19.633 19 21.927 12.383 21.948 12.316L22.054 12L21.949 11.684C21.927 11.617 19.633 5 12 5ZM12 17C6.64901 17 4.57601 13.154 4.07401 12C4.57801 10.842 6.65201 7 12 7C17.351 7 19.424 10.846 19.926 12C19.422 13.158 17.348 17 12 17Z" fill="url(#paint1_linear_181_334)"/>
                    <defs>
                      <linearGradient id="paint0_linear_181_334" x1="12" y1="9" x2="12" y2="15" gradientUnits="userSpaceOnUse">
                        <stop stop-color="#161A3E"/>
                        <stop offset="1" stop-color="#3A45A4"/>
                      </linearGradient>
                      <linearGradient id="paint1_linear_181_334" x1="2" y1="12" x2="22" y2="12" gradientUnits="userSpaceOnUse">
                        <stop stop-color="#161A3E"/>
                        <stop offset="1" stop-color="#3A45A4"/>
                      </linearGradient>
                    </defs>
                  </svg>
                ')
              )
            )
          ),
          
          actionButton(
            "login_btn",
            label = "Login",
            class = "btn-login"
          )
        )
      )
    )
  ),
  
  shinyjs::hidden(
    div(id = "main_panel", class = "container",   
      div(class = "header-row",
        div(class = "header-title",
            "Gacha Pull", br(),
            span(class = "subtitle", 
                  "Logger"
            )
        )
      ),

      # Input Fields Card
      div(class = "panel-wrapper",
        div(class="logout-container", 
          actionButton(
            "logout_btn",
            label = tagList(
              HTML('
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none">
                  <path d="M16 13V11H7V8L2 12L7 16V13H16Z" fill="white"/>
                  <path d="M20 3H11C9.897 3 9 3.897 9 5V9H11V5H20V19H11V15H9V19C9 20.103 9.897 21 11 21H20C21.103 21 22 20.103 22 19V5C22 3.897 21.103 3 20 3Z" fill="white"/>
                </svg>
              '),
              "Logout"
            ),
            class = "btn-logout"
          )
        ),
        tabsetPanel(
          id = "main_tabs",
          type = "tabs",
          
          tabPanel(
            title = "Logger",
            value = "logger",
            div(class = "logger-content",
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
            )
          ),
        
          tabPanel(
            title = "Analytics",
            value = "analytics",
            div(class = "analytics-content",
              div(class = "analytics-left",
                div(class = "stats-card overall-stats",
                  div(class = "stats-card-title",
                    div(class = "stats-card-title-text", "Overall Statistics"),  
                    tags$span(
                      class = "info-icon",
                      title="• Pulls & Primogems: All banners\n• Avg Pity & Luck: Selected banner",
                      HTML('
                        <svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none">
                          <path d="M9 1.5C4.8645 1.5 1.5 4.8645 1.5 9C1.5 13.1355 4.8645 16.5 9 16.5C13.1355 16.5 16.5 13.1355 16.5 9C16.5 4.8645 13.1355 1.5 9 1.5ZM9.75 12.75H8.25V8.25H9.75V12.75ZM9.75 6.75H8.25V5.25H9.75V6.75Z" fill="#F3EFE4"/>
                        </svg>
                      ')
                    )
                  ),
                  div(class = "stats-list",
                    div(class = "stats-column",
                      div(class = "stat-row",
                        span(class = "stat-label", "Total Pulls:"),
                        span(class = "stat-value", textOutput("total_pulls", inline = TRUE))
                      ),
                      div(class = "stat-row",
                        span(class = "stat-label", "Primogems Spent:"),
                        span(class = "stat-value", textOutput("primogems_spent", inline = TRUE))
                      ),
                      div(class = "stats-divider", 
                        div(class = "divider-line")
                      )
                    ),
                    div(class = "stats-column",
                      div(class = "stat-row",
                        span(class = "stat-label", "Average Pity:"),
                        span(class = "stat-value", textOutput("avg_pity", inline = TRUE))
                      ),
                      div(class = "stat-row",
                        span(class = "stat-label", "Luck Index:"),
                        span(class = "stat-value", 
                          textOutput("luck_index", inline = TRUE),
                          tags$span(
                            class = "info-icon",
                            title = "Luck Index per Banner\n\nHow early you get 5★s compared to the game's expected pity.\n\n> 1.0  Earlier than expected\n= 1.0  As expected\n< 1.0  Later than expected",
                            HTML('
                              <svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" viewBox="0 0 15 15" fill="none">
                                <g clip-path="url(#clip0_251_211)">
                                  <path d="M7.5 10V7.5M7.5 5H7.50625M13.75 7.5C13.75 10.9518 10.9518 13.75 7.5 13.75C4.04822 13.75 1.25 10.9518 1.25 7.5C1.25 4.04822 4.04822 1.25 7.5 1.25C10.9518 1.25 13.75 4.04822 13.75 7.5Z" stroke="#1E1E1E" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                                </g>
                                <defs>
                                  <clipPath id="clip0_251_211">
                                    <rect width="15" height="15" fill="white"/>
                                  </clipPath>
                                </defs>
                              </svg>
                            ')
                          )
                        )
                      )
                    )
                  )
                ),
                div(class = "stats-card recent-pulls-card",
                  div(class = "stats-card-header",
                    HTML('
                      <svg xmlns="http://www.w3.org/2000/svg" width="19" height="15" viewBox="0 0 19 15" fill="none">
                        <path d="M6.54015 0.460536C6.48 0.410343 6.40729 0.378667 6.33103 0.369418C6.25476 0.36017 6.17826 0.373753 6.11098 0.408493L4.14356 1.42583L2.16795 0.0712252C2.10461 0.0274134 2.03078 0.00275185 1.95536 0.00021715C1.87995 -0.00231755 1.80617 0.0173828 1.74294 0.0569407C1.6797 0.0964986 1.62972 0.154223 1.59901 0.223148C1.5683 0.292073 1.55818 0.369252 1.56988 0.445371L1.92668 2.81443L0.152716 4.14061C0.0931037 4.18516 0.0479715 4.24667 0.0227189 4.31779C-0.00253367 4.38892 -0.00683747 4.46664 0.0103225 4.54167C0.0274824 4.61669 0.0653795 4.68584 0.11948 4.74084C0.17358 4.79584 0.241593 4.83437 0.315381 4.8518L2.57479 5.38596L3.46358 7.8917C3.49043 7.96757 3.53884 8.03427 3.6025 8.08313C3.66617 8.13199 3.74216 8.16076 3.82061 8.1657C3.89906 8.17065 3.97636 8.15154 4.04246 8.11086C4.10857 8.07017 4.16043 8.0098 4.19133 7.93756L5.2029 5.58095L7.76471 5.69927C7.84476 5.70282 7.9231 5.68138 7.98922 5.63783C8.05534 5.59429 8.10605 5.53074 8.13455 5.45572C8.16304 5.3807 8.16794 5.29781 8.14859 5.2182C8.12925 5.1386 8.08658 5.06609 8.02633 5.01044L6.05956 3.2046L6.67634 0.880734C6.71657 0.728329 6.66308 0.563429 6.54015 0.460536Z" fill="#171B41"/>
                        <path d="M18.6387 9.18034C18.6539 9.0839 18.6412 8.9859 18.6023 8.89844C18.5634 8.81097 18.5 8.73787 18.4198 8.68815L16.0734 7.23527L16.258 4.2563C16.2643 4.16054 16.2435 4.06582 16.1979 3.98356C16.1524 3.9013 16.0841 3.83502 16.0014 3.79271C15.9186 3.7504 15.8249 3.73388 15.7316 3.74514C15.6383 3.75639 15.5493 3.79495 15.4753 3.85615L13.1705 5.75317L10.6176 4.70473C10.5319 4.66948 10.4372 4.66057 10.345 4.67907C10.2528 4.69757 10.1669 4.74269 10.0977 4.80903C10.0285 4.87537 9.97877 4.96013 9.95457 5.05316C9.93036 5.14619 9.93268 5.24356 9.96124 5.33361L10.8355 8.0912L8.72475 10.6445C8.6608 10.7217 8.62045 10.8162 8.60894 10.9155C8.59742 11.0148 8.61527 11.1145 8.66017 11.2015C8.70507 11.2886 8.77495 11.359 8.86074 11.4037C8.94653 11.4483 9.04426 11.4652 9.14126 11.452L12.3082 11.0261L13.8205 13.841C13.8679 13.9288 13.941 13.9989 14.0298 14.0417C14.1187 14.0846 14.2191 14.0981 14.3175 14.0806C14.4159 14.0631 14.5077 14.0153 14.5805 13.9437C14.6532 13.8721 14.7034 13.7801 14.7244 13.6801L15.3971 10.4219L18.2765 9.59476C18.4652 9.54028 18.6073 9.3776 18.6387 9.18034Z" fill="#171B41"/>
                      </svg>
                    '),
                    span("Recent Pulls")
                  ),
                  uiOutput("recent_pulls_list")
                )
              ),
              div(class = "analytics-right",
                div(class = "stats-card current-progress",
                  div(class = "progress-header",
                    div(class = "progress-title",
                      HTML('
                        <svg xmlns="http://www.w3.org/2000/svg" width="26" height="24" viewBox="0 0 26 24" fill="none">
                          <path d="M13.5583 6H15.6442V17H13.5583V6ZM17.7301 3H19.816V17H17.7301V3ZM9.3865 9H11.4724V17H9.3865V9ZM4.17178 19H20.8589V21H4.17178V19ZM5.21472 12H7.30061V17H5.21472V12Z" fill="#161A3E"/>
                        </svg>
                      '),
                      span("Current Progress")
                    ),
                    selectInput(
                      inputId = "progress_banner",
                      label = NULL,
                      choices = c("Character Event Wish", "Weapon Event Wish", "Standard Event Wish"),
                      selected = "Character Event Wish"
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
                        span(class = "pity-max", textOutput("pity_threshold", inline = TRUE))
                      )
                    ),
                    div(class = "pity-bar-container",
                      div(class = "pity-bar-bg",
                        div(class = "pity-bar-fill", style = "width: 0%", id = "pity-fill"),
                        # Soft pity indicator SVG at 74 or 64
                        span(class = "soft-pity-indicator",
                          HTML('
                            <svg xmlns="http://www.w3.org/2000/svg" width="4" height="9" viewBox="0 0 1 9" fill="none">
                              <path d="M0.25 0L0.25 9" stroke="#161A3E" stroke-width="0.5"/>
                            </svg>
                          ')
                        )
                      ),
                      div(class = "pity-markers",
                        span(class = "marker", "0"),
                        span(class = "marker soft-pity", id = "soft-pity-marker-text", textOutput("soft_pity_marker", inline = TRUE)),
                        span(class = "marker hard-pity", textOutput("hard_pity_marker", inline = TRUE))
                      )
                    )
                  )
                ),

                div(class = "stats-card pull-trends",
                  div(class = "stats-card-header",
                    div(class = "header-left",
                      HTML('
                        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none">
                          <path d="M3 3V20C3 20.2652 3.10536 20.5196 3.29289 20.7071C3.48043 20.8946 3.73478 21 4 21H21V19H5V3H3Z" fill="#161A3E"/>
                          <path d="M15.293 14.707C15.3858 14.7999 15.496 14.8737 15.6173 14.924C15.7386 14.9743 15.8687 15.0002 16 15.0002C16.1313 15.0002 16.2614 14.9743 16.3827 14.924C16.504 14.8737 16.6142 14.7999 16.707 14.707L21.707 9.70697L20.293 8.29297L16 12.586L13.707 10.293C13.6142 10.2 13.504 10.1263 13.3827 10.076C13.2614 10.0257 13.1313 9.99977 13 9.99977C12.8687 9.99977 12.7386 10.0257 12.6173 10.076C12.496 10.1263 12.3858 10.2 12.293 10.293L7.293 15.293L8.707 16.707L13 12.414L15.293 14.707Z" fill="#161A3E"/>
                        </svg>
                      '),
                      span("Pull Trends")
                    ),
                    actionButton(
                      "import_data",
                      label = tagList(
                        HTML('
                          <svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none">
                            <g clip-path="url(#clip0_253_262)">
                              <path d="M10.5842 0.00745044L0.298469 1.92035C0.124841 1.95271 0 2.10743 0 2.28043V15.7158C0 15.8888 0.124841 16.0435 0.298469 16.0758L10.5842 17.9887C10.6071 17.993 10.6301 18 10.6531 18C10.7377 18 10.8166 17.9747 10.8827 17.9212C10.9673 17.8523 11.0204 17.7468 11.0204 17.6399V0.356274C11.0204 0.249377 10.9673 0.143885 10.8827 0.0749647C10.798 0.00604391 10.6918 -0.0122412 10.5842 0.00745044ZM11.7551 2.15665V4.67719H12.4898V5.39734H11.7551V7.19772H12.4898V7.91787H11.7551V9.71825H12.4898V10.4384H11.7551V12.5989H12.4898V13.319H11.7551V15.8395H17.2653C17.67 15.8395 18 15.516 18 15.1194V2.87681C18 2.48016 17.67 2.15665 17.2653 2.15665H11.7551ZM13.2245 4.67719H16.1633V5.39734H13.2245V4.67719ZM2.45663 5.64489H4.33929L5.32653 7.65907C5.40402 7.81801 5.4729 8.01352 5.53316 8.23294H5.54464C5.58339 8.10213 5.658 7.89677 5.76276 7.63656L6.85332 5.64489H8.57526L6.52041 8.97559L8.63265 12.3738H6.8074L5.61352 10.1796C5.56904 10.098 5.52312 9.94752 5.47577 9.7295H5.46429C5.44133 9.83218 5.3868 9.99253 5.30357 10.2021L4.10969 12.3738H2.27296L4.46556 9.00935L2.45663 5.64489ZM13.2245 7.19772H16.1633V7.91787H13.2245V7.19772ZM13.2245 9.71825H16.1633V10.4384H13.2245V9.71825ZM13.2245 12.5989H16.1633V13.319H13.2245V12.5989Z" fill="currentColor"/>
                            </g>
                            <defs>
                              <clipPath id="clip0_253_262">
                                <rect width="18" height="18" fill="white"/>
                              </clipPath>
                            </defs>
                          </svg>
                        '),
                        "Import"
                      ),
                      class = "btn-import"
                    )
                  ),
                  div(class = "trends-chart-area",
                    plotlyOutput("pull_trends_chart", width = "100%", height = "277.163px")
                  )
                )
              )
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
  logged_in <- reactiveVal(FALSE)
  auth_checked <- reactiveVal(FALSE)
  
  # Show/hide panels based on auth state (server-controlled)
  observe({
    if (auth_checked()) {
      if (logged_in()) {
        shinyjs::hide("login_panel")
        shinyjs::show("main_panel")
      } else {
        shinyjs::show("login_panel")
        shinyjs::hide("main_panel")
      }

      shinyjs::runjs("
        var loader = $('.loader-overlay');
        if (loader.length && !loader.hasClass('slide-up')) {
          loader.addClass('slide-up');
          setTimeout(function() {
            loader.hide();
          }, 500);
        }
      ")
    }
  })
  
  observeEvent(input$auth_ready, {
    auth_checked(TRUE)
  }, once = TRUE, ignoreInit = TRUE)
  
  # Login handler
  observeEvent(input$login_btn, {
    username <- input$login_username
    password <- input$login_password
  
    if (identical(username, "Kurt013") && identical(password, "@GenshinImpact13")) {
      logged_in(TRUE)
      session$sendCustomMessage("saveLoginState", list(logged_in = TRUE))
      shinytoastr::toastr_success("Welcome, Traveler!", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    } else {
      shinytoastr::toastr_error("Invalid username or password.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    }
  })

  observeEvent(input$restore_session, {
    logged_in(TRUE)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Logout handler
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    session$sendCustomMessage("saveLoginState", list(logged_in = FALSE))
    shinytoastr::toastr_info("You have been logged out.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
  })

  data <- reactiveVal(data.frame())

  # ---- Customize Data table ----
  output$table <- renderDT({
    req(logged_in())
    
    df <- data()

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

  observeEvent(input$table_rows_selected, {
    req(logged_in())
    df <- data()
    
    banner_filter <- input$filter
    if (!is.null(banner_filter) && banner_filter != "All (Banner)") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }
    
    selected_row <- input$table_rows_selected
    
    if (is.null(selected_row) || length(selected_row) == 0) {
      clear_fields()
      return()
    }
    
    row <- df[selected_row, ]
    
    updateSelectInput(session, "type", selected = row$type)
    updateTextInput(session, "name", value = row$name)
    updateSelectInput(session, "rarity", selected = row$rarity)
    updateSelectInput(session, "banner", selected = row$banner)
    updateDateInput(session, "date", value = as.Date(row$pull_date))
  }, ignoreNULL = FALSE)
  
  # ----- CRUD FUNCTIONALITY -----
  # ---- ADD (Create) ----
  observeEvent(input$add, {
    req(logged_in())
    
    # Validate all fields
    if (is.null(input$type) || !nzchar(input$type) ||
        is.null(input$name) || !nzchar(input$name) ||
        is.null(input$rarity) || !nzchar(input$rarity) ||
        is.null(input$banner) || !nzchar(input$banner) ||
        is.null(input$date)) {
      shinytoastr::toastr_warning("Please fill in all fields.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
      return()
    }
    
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
        format(input$date, '%Y-%m-%d'),
        pity
      )
    )
  
    if (result == 1) {
      shinytoastr::toastr_success("Pull added successfully!", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    } else {
      shinytoastr::toastr_error("Failed to add pull.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    }

    clear_fields()
    recalculate_all_pity(conn)
    refresh()
  })

  # ---- READ ----
  refresh <- function() {
    if (!logged_in()) return()
    
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

  # Initial refresh after login
  observeEvent(logged_in(), {
    if (logged_in()) refresh()
  })
  
  # ---- UPDATE ----
  observeEvent(input$update, {
    req(logged_in())
    
    if (is.null(input$table_rows_selected) || length(input$table_rows_selected) == 0) {
      shinytoastr::toastr_warning("Please select a row to update.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
      return()
    }
    
    # Validate all fields
    if (is.null(input$type) || !nzchar(input$type) ||
        is.null(input$name) || !nzchar(input$name) ||
        is.null(input$rarity) || !nzchar(input$rarity) ||
        is.null(input$banner) || !nzchar(input$banner) ||
        is.null(input$date)) {
      shinytoastr::toastr_warning("Please fill in all fields.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
      return()
    }
    
    conn <- conn_db()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    df <- data()

    banner_filter <- input$filter
    if (!is.null(banner_filter) && banner_filter != "All (Banner)") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }

    selected_row <- input$table_rows_selected
    if (length(selected_row) == 0) return()
    row_id <- df$id[selected_row]
    
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
      "UPDATE pulls SET banner_id = ?, type = ?, name = ?, rarity = ?, pity = ?
      WHERE id = ?",
      list(
        banner_id,
        input$type,
        input$name,
        input$rarity,
        format(input$date, '%Y-%m-%d'),
        row_id
      )
    )

    if (result == 1) {
      # Success
      shinytoastr::toastr_success("Pull updated successfully!", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    } else {
      # Failure
      shinytoastr::toastr_error("Failed to update pull.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    }
    
    clear_fields()
    recalculate_all_pity(conn)
    refresh()
  })

  # ---- DELETE ----
  observeEvent(input$delete, {
    req(logged_in())
    
    if (is.null(input$table_rows_selected) || length(input$table_rows_selected) == 0) {
      shinytoastr::toastr_warning("Please select a row to delete.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
      return()
    }
    
    conn <- conn_db()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    df <- data()
    banner_filter <- input$filter
    if (!is.null(banner_filter) && banner_filter != "All (Banner)") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }
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
      shinytoastr::toastr_success("Pull deleted successfully!", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    } else {
      # Failure
      shinytoastr::toastr_error("Failed to delete pull.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
    }

    clear_fields()
    recalculate_all_pity(conn)
    refresh()
  })

  observeEvent(input$clear, {
    req(logged_in())
    clear_fields()
    shinytoastr::toastr_info("Form cleared.", progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE)
  })

  observeEvent(input$import_data, {
    req(logged_in())

    existing_count <- nrow(data())
    
    showModal(modalDialog(
      title = div(class = "import-modal-header",
        HTML('
          <svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none">
            <g clip-path="url(#clip0_253_262)">
              <path d="M10.5842 0.00745044L0.298469 1.92035C0.124841 1.95271 0 2.10743 0 2.28043V15.7158C0 15.8888 0.124841 16.0435 0.298469 16.0758L10.5842 17.9887C10.6071 17.993 10.6301 18 10.6531 18C10.7377 18 10.8166 17.9747 10.8827 17.9212C10.9673 17.8523 11.0204 17.7468 11.0204 17.6399V0.356274C11.0204 0.249377 10.9673 0.143885 10.8827 0.0749647C10.798 0.00604391 10.6918 -0.0122412 10.5842 0.00745044ZM11.7551 2.15665V4.67719H12.4898V5.39734H11.7551V7.19772H12.4898V7.91787H11.7551V9.71825H12.4898V10.4384H11.7551V12.5989H12.4898V13.319H11.7551V15.8395H17.2653C17.67 15.8395 18 15.516 18 15.1194V2.87681C18 2.48016 17.67 2.15665 17.2653 2.15665H11.7551ZM13.2245 4.67719H16.1633V5.39734H13.2245V4.67719ZM2.45663 5.64489H4.33929L5.32653 7.65907C5.40402 7.81801 5.4729 8.01352 5.53316 8.23294H5.54464C5.58339 8.10213 5.658 7.89677 5.76276 7.63656L6.85332 5.64489H8.57526L6.52041 8.97559L8.63265 12.3738H6.8074L5.61352 10.1796C5.56904 10.098 5.52312 9.94752 5.47577 9.7295H5.46429C5.44133 9.83218 5.3868 9.99253 5.30357 10.2021L4.10969 12.3738H2.27296L4.46556 9.00935L2.45663 5.64489ZM13.2245 7.19772H16.1633V7.91787H13.2245V7.19772ZM13.2245 9.71825H16.1633V10.4384H13.2245V9.71825ZM13.2245 12.5989H16.1633V13.319H13.2245V12.5989Z" fill="currentColor"/>
            </g>
            <defs>
              <clipPath id="clip0_253_262">
                <rect width="18" height="18" fill="white"/>
              </clipPath>
            </defs>
          </svg>
        '),
        span("Import from Excel")
      ),
      div(class = "import-modal-content",
        if (existing_count > 0) {
          div(class = "existing-data-warning",
            div(class = "warning-header",
              HTML('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                <circle cx="12" cy="12" r="10"></circle>
                <line x1="12" y1="8" x2="12" y2="12"></line>
                <line x1="12" y1="16" x2="12.01" y2="16"></line>
              </svg>'),
              span(paste0("You have ", format(existing_count, big.mark = ","), " existing pull records."))
            ),
            checkboxInput(
              "delete_before_import",
              label = "Delete all existing data before importing",
              value = FALSE
            )
          )
        },
        div(class = "import-instructions",
          p("Upload your pull history records (.xlsx)."),
          p(class = "import-note", "Only Character Event, Weapon Event, and Standard Event Banners will be imported.")
        ),
        fileInput(
          "import_file",
          label = NULL,
          accept = c(".xlsx", ".xls"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        div(class = "import-preview",
          uiOutput("import_preview_table")
        )
      ),
      footer = tagList(
        actionButton("cancel_import", "Cancel", class = "btn-modal-cancel"),
        actionButton("confirm_import", "Import", class = "btn-modal-import")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  output$import_preview_table <- renderUI({
    req(logged_in())
    req(input$import_file)
    
    tryCatch({
      all_sheets <- excel_sheets(input$import_file$datapath)
      
      supported_sheets <- c("Character Event", "Weapon Event", "Standard")
      banner_map <- list(
        "Character Event" = "Character Event Wish",
        "Weapon Event" = "Weapon Event Wish",
        "Standard" = "Standard Wish"
      )
      
      sheets <- intersect(all_sheets, supported_sheets)
      
      if (length(sheets) == 0) {
        return(div(class = "import-error", "No supported sheets found. Expected: Character Event, Weapon Event, or Standard."))
      }
      
      sheet_info <- lapply(sheets, function(sheet) {
        df <- read_excel(input$import_file$datapath, sheet = sheet)
        list(sheet = sheet, banner = banner_map[[sheet]], rows = nrow(df))
      })
      
      total_rows <- sum(sapply(sheet_info, function(x) x$rows))
      
      div(
        p(class = "preview-label", paste("Found", total_rows, "pulls to import:")),
        tags$table(class = "preview-table",
          tags$thead(
            tags$tr(
              tags$th("Sheet"),
              tags$th("Banner"),
              tags$th("Pulls")
            )
          ),
          tags$tbody(
            lapply(sheet_info, function(info) {
              tags$tr(
                tags$td(info$sheet),
                tags$td(info$banner),
                tags$td(info$rows)
              )
            })
          )
        )
      )
    }, error = function(e) {
      div(class = "import-error", paste("Error reading file:", e$message))
    })
  })
  
  observeEvent(input$cancel_import, {
    req(logged_in())
    removeModal()
  })
  
  observeEvent(input$confirm_import, {
    req(logged_in())
    req(input$import_file)
    
    shinyjs::disable("confirm_import")
    
    withProgress(message = "Importing pulls...", value = 0, {
      tryCatch({
        if (!is.null(input$delete_before_import) && input$delete_before_import == TRUE) {
          conn_del <- conn_db()
          dbExecute(conn_del, "DELETE FROM pulls")
          dbExecute(conn_del, "DELETE FROM banners")
          dbDisconnect(conn_del)
        }
        
        all_sheets <- excel_sheets(input$import_file$datapath)
        
        supported_sheets <- c("Character Event", "Weapon Event", "Standard")
        banner_map <- list(
          "Character Event" = "Character Event Wish",
          "Weapon Event" = "Weapon Event Wish",
          "Standard" = "Standard Event Wish"
        )
        
        sheets <- intersect(all_sheets, supported_sheets)
        
        if (length(sheets) == 0) {
          shinytoastr::toastr_error(
            "No supported sheets found in file.",
            progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE
          )
          return()
        }
        
        conn <- conn_db()
        on.exit(dbDisconnect(conn), add = TRUE)
        
        imported_count <- 0
        total_sheets <- length(sheets)
        
        for (sheet_idx in seq_along(sheets)) {
          sheet <- sheets[sheet_idx]
          
          incProgress(1 / total_sheets, detail = paste("Processing", sheet, "..."))
          
          df <- read_excel(input$import_file$datapath, sheet = sheet)
          
          if (nrow(df) == 0) next
          
          banner_name <- banner_map[[sheet]]
          
          dbExecute(conn, "INSERT OR IGNORE INTO banners (banner_name) VALUES (?)", banner_name)
          
          banner_id <- dbGetQuery(conn, "SELECT banner_id FROM banners WHERE banner_name = ?", banner_name)$banner_id
          
          # Paimon.moe columns: Type, Name, Time, ⭐
          col_names <- names(df)
          
          name_col <- col_names[grepl("^name$", col_names, ignore.case = TRUE)][1]
          type_col <- col_names[grepl("^type$", col_names, ignore.case = TRUE)][1]
          rarity_col <- col_names[grepl("⭐|star|rarity", col_names, ignore.case = TRUE)][1]
          date_col <- col_names[grepl("time|date", col_names, ignore.case = TRUE)][1]
          
          if (is.na(name_col)) {
            shinytoastr::toastr_warning(
              paste("Sheet", sheet, "missing 'Name' column, skipping..."),
              progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE
            )
            next
          }
          
          for (i in seq_len(nrow(df))) {
            row <- df[i, ]
            
            item_name <- as.character(row[[name_col]])
            item_type <- if (!is.na(type_col)) as.character(row[[type_col]]) else "Unknown"
            
            if (!is.na(rarity_col)) {
              rarity_val <- row[[rarity_col]]
              if (is.numeric(rarity_val)) {
                item_rarity <- paste0(as.integer(rarity_val), "-Star")
              } else {
                item_rarity <- as.character(rarity_val)
                if (grepl("^[345]$", item_rarity)) {
                  item_rarity <- paste0(item_rarity, "-Star")
                }
              }
            } else {
              item_rarity <- "3-Star"
            }
            
            if (!is.na(date_col)) {
              date_val <- row[[date_col]]
              item_date <- format(as.Date(date_val), '%Y-%m-%d')
            } else {
              item_date <- format(Sys.Date(), '%Y-%m-%d')
            }
            
            pity <- compute_pity(conn, banner_name)
            
            result <- dbExecute(
              conn,
              "INSERT INTO pulls (banner_id, type, name, rarity, pull_date, pity) VALUES (?, ?, ?, ?, ?, ?)",
              list(banner_id, item_type, item_name, item_rarity, item_date, pity)
            )
            
            if (result == 1) imported_count <- imported_count + 1
          }
        }
        
        incProgress(0, detail = "Recalculating pity...")
        recalculate_all_pity(conn)
        
        removeModal()
        refresh()
        
        shinytoastr::toastr_success(
          paste("Successfully imported", imported_count, "pulls!"),
          progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE
        )
        
      }, error = function(e) {
        shinyjs::enable("confirm_import")
        shinytoastr::toastr_error(
          paste("Import failed:", e$message),
          progressBar = TRUE, showMethod = "slideDown", preventDuplicates = TRUE
        )
      })
    })
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
  output$total_pulls <- renderText({
    req(logged_in())
    df <- data()

    pulls <- if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      0
    } else {
      nrow(df)
    }

    format(pulls, big.mark = ",")
  })

  # 160 primogems per pull
  output$primogems_spent <- renderText({
    req(logged_in())
    df <- data()

    if (is.null(df) || nrow(df) == 0) {
      return("0")
    }

    format(nrow(df) * 160, big.mark = ",")
  })

  # Expected pity / average pity)
  output$luck_index <- renderText({
    req(logged_in())

    df <- data()

    banner_filter <- input$progress_banner
    if (!is.null(banner_filter) && banner_filter != "") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }

    if (is.null(df) || nrow(df) == 0) {
      return("N/A")
    }

    five_stars <- df[df$rarity == "5-Star", ]
    if (nrow(five_stars) == 0) return("N/A")
    avg_pity <- mean(five_stars$pity, na.rm = TRUE)

    exp_pity <- 75
    if (banner_filter == "Weapon Event Wish") {
      exp_pity <- 65
    }

    luck_index <- exp_pity / avg_pity
    if (!is.finite(luck_index) || is.nan(luck_index)) return("N/A")
    sprintf("%.2f×", luck_index)
  })

  output$avg_pity <- renderText({
    req(logged_in())
    df <- data()

    banner_filter <- input$progress_banner
    if (!is.null(banner_filter) && banner_filter != "") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }

    five_stars <- df[df$rarity == "5-Star", ]
    if (nrow(five_stars) == 0) return("N/A")
    round(mean(five_stars$pity, na.rm = TRUE))
  })

  output$current_pity <- renderText({
    req(logged_in())
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) return(0)
    banner_df <- df[df$banner == banner_filter, ]
    if (nrow(banner_df) == 0) return(0)

    latest_row <- banner_df[1, ]

    if (latest_row$rarity == "5-Star") {
      return(0)
    }

    latest_pity <- latest_row$pity
    if (is.null(latest_pity) || is.na(latest_pity)) return(0)
    latest_pity
  })

  output$pity_pulls_ago <- renderText({
    req(logged_in())
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) return(90)
    banner_df <- df[df$banner == banner_filter, ]
    if (nrow(banner_df) == 0) return(90)

    latest_row <- banner_df[1, ]

    if (latest_row$rarity == "5-Star") {
      return(90)
    }

    latest_pity <- latest_row$pity
    if (is.null(latest_pity) || is.na(latest_pity)) return(90)
    pulls_left <- 90 - latest_pity
    pulls_left
  })

  # Update pity bar fill width
  observe({
    req(logged_in())
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) {
      pity <- 0
    } else {
      banner_df <- df[df$banner == banner_filter, ]
      if (nrow(banner_df) == 0) {
        pity <- 0
      } else {
        latest_row <- banner_df[1, ]
        if (latest_row$rarity == "5-Star") {
          pity <- 0
        } else {
          pity <- latest_row$pity
          if (is.null(pity) || is.na(pity)) pity <- 0
        }
      }
    }

    thresholds <- get_pity_thresholds(banner_filter)
    pity_threshold <- thresholds$hard

    pct <- min(100, max(0, (pity / pity_threshold) * 100))
    session$sendCustomMessage("updatePityBar", list(width = pct))
  })

  observe({
    req(logged_in())
    banner_filter <- input$progress_banner
    thresholds <- get_pity_thresholds(banner_filter)
    soft_pity <- thresholds$soft
    hard_pity <- thresholds$hard
    percent <- round((soft_pity / hard_pity) * 100, 2)
    session$sendCustomMessage("updateSoftPityMarker", list(percent = percent))
  })

  output$soft_pity_marker <- renderText({
    thresholds <- get_pity_thresholds(input$progress_banner)
    paste0("Soft Pity (", thresholds$soft, ")")
  })

  output$hard_pity_marker <- renderText({
    thresholds <- get_pity_thresholds(input$progress_banner)
    paste0("Hard Pity (", thresholds$hard, ")")
  })

  output$pity_threshold <- renderText({
    thresholds <- get_pity_thresholds(input$progress_banner)
    paste0(" / ", thresholds$hard)
  })

  output$recent_pulls_list <- renderUI({
    req(logged_in())
    df <- data()

    banner_filter <- input$progress_banner
    if (!is.null(banner_filter) && banner_filter != "All (Banner)") {
      df <- df[df$banner == banner_filter, , drop = FALSE]
    }
    
    if (nrow(df) == 0) {
      return(div(class = "no-pulls", "No rare pulls recorded yet"))
    }
    
    five_star_pulls <- df[df$rarity == "5-Star", ]
    five_star_pulls <- head(five_star_pulls, 5)
    
    remaining_slots <- 5 - nrow(five_star_pulls)
    four_star_pulls <- data.frame()
    
    if (remaining_slots > 0) {
      four_star_pulls <- df[df$rarity == "4-Star", ]
      four_star_pulls <- head(four_star_pulls, remaining_slots)
    }
    
    rare_pulls <- rbind(five_star_pulls, four_star_pulls)
    
    if (nrow(rare_pulls) == 0) {
      return(div(class = "no-pulls", "No rare pulls recorded yet"))
    }
    
    star_svg <- HTML('
      <svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 18 18" fill="none" style="vertical-align:middle;">
        <path d="M17.4536 6.28335C17.3987 6.12135 17.2974 5.97901 17.1624 5.8739C17.0275 5.76878 16.8647 5.7055 16.6941 5.69185L11.7058 5.29547L9.54715 0.517097C9.4784 0.363195 9.36658 0.232478 9.22519 0.140722C9.08379 0.048966 8.91887 9.26441e-05 8.75031 1.31581e-07C8.58175 -9.2381e-05 8.41677 0.0486 8.27527 0.140201C8.13377 0.231801 8.02181 0.362396 7.9529 0.516222L5.79427 5.29547L0.805898 5.69185C0.638296 5.70512 0.478069 5.76641 0.344384 5.86836C0.210698 5.97032 0.109217 6.10863 0.0520795 6.26675C-0.00505783 6.42487 -0.0154315 6.5961 0.0221998 6.75996C0.0598311 6.92382 0.143874 7.07337 0.264273 7.19072L3.95065 10.7843L2.6469 16.4298C2.60731 16.6007 2.62 16.7796 2.68332 16.9432C2.74663 17.1067 2.85765 17.2475 3.00198 17.3472C3.1463 17.4469 3.31725 17.501 3.49266 17.5023C3.66807 17.5036 3.83983 17.4522 3.98565 17.3547L8.75002 14.1785L13.5144 17.3547C13.6634 17.4537 13.8392 17.5046 14.0181 17.5007C14.1969 17.4968 14.3703 17.4382 14.5149 17.3328C14.6594 17.2274 14.7683 17.0802 14.8267 16.9112C14.8851 16.7421 14.8904 16.5591 14.8418 16.387L13.2414 10.787L17.2104 7.21522C17.4703 6.98072 17.5657 6.61497 17.4536 6.28335Z" fill="currentColor"/>
      </svg>
    ')
    
    pull_items <- lapply(seq_len(nrow(rare_pulls)), function(i) {
      row <- rare_pulls[i, ]
      star_count <- if (row$rarity == "5-Star") 5 else 4
      stars_html <- HTML(paste(rep(as.character(star_svg), star_count), collapse = ""))
      border_color <- if (row$rarity == "5-Star") "gold" else "purple"
      
      if (row$rarity == "5-Star") {
        pity_display <- row$pity
      } else {
        # 4-star rarity: 1-10 pity
        pity_mod <- row$pity %% 10
        pity_display <- if (pity_mod == 0) 10 else pity_mod
      }
      
      div(
        class = paste("recent-pull-item", border_color),
        `data-name` = row$name,
        `data-type` = row$type,
        `data-rarity` = row$rarity,
        `data-date` = row$pull_date,
        `data-pity` = pity_display,
        span(class = "pull-stars", stars_html),
        span(class = "pull-name", row$name),
        span(class = paste("pull-pity", if (row$rarity == "5-Star") "stars-5" else "stars-4"), pity_display)
      )
    })
    do.call(tagList, pull_items)
  })

  output$pull_trends_chart <- renderPlotly({
    req(logged_in())
    df <- data()
    if (nrow(df) == 0) {
      plotly_empty(type = "scatter", mode = "lines") %>%
        layout(
          xaxis = list(
            title = "Month",
            type = "category",
            titlefont = list(
              family = "ContentFont, sans-serif",
              color = "#F3EFE4"
            )
          ),
          yaxis = list(
            title = "Number of Pulls",
              titlefont = list(
              family = "ContentFont, sans-serif",
              color = "#F3EFE4"
            )
          ),
          plot_bgcolor = "#161A3E",
          paper_bgcolor = "#161A3E",
          font = list(
            color = "#F3EFE4",
            family = "Inter, sans-serif",
            weight = "normal"
          ),
          annotations = list(
            list(
              text = "No data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )
          )
        )
    } else {
      banner_filter <- input$progress_banner
      if (!is.null(banner_filter) && banner_filter != "") {
        df <- df[df$banner == banner_filter, , drop = FALSE]
      }
      if (nrow(df) == 0) {
        plotly_empty(type = "scatter", mode = "lines") %>%
          layout(
            xaxis = list(
              title = "Month",
              type = "category",
              titlefont = list(
                family = "ContentFont, sans-serif",
                color = "#F3EFE4"
              )
            ),
            yaxis = list(
              title = "Number of Pulls",
                titlefont = list(
                family = "ContentFont, sans-serif",
                color = "#F3EFE4"
              )
            ),
            plot_bgcolor = "#161A3E",
            paper_bgcolor = "#161A3E",
            font = list(
              color = "#F3EFE4",
              family = "Inter, sans-serif",
              weight = "normal"
            ),   
            annotations = list(
              list(
                text = "No data for selected banner",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, showarrow = FALSE
              )
            )
          )
      } else {
        banner_filter <- input$progress_banner
        if (!is.null(banner_filter) && banner_filter != "") {
          df <- df[df$banner == banner_filter, , drop = FALSE]
        }

        df$month <- format(as.Date(df$pull_date), "%Y-%m")

        agg <- df %>%
          group_by(month, rarity) %>%
          summarise(count = n(), .groups = "drop")

        all_months <- sort(unique(df$month))
        all_rarities <- c("3-Star", "4-Star", "5-Star")
        complete_agg <- tidyr::complete(
          agg,
          month = all_months,
          rarity = all_rarities,
          fill = list(count = 0)
        )

        plot_ly(complete_agg, x = ~month) %>%
          add_trace(
            data = subset(complete_agg, rarity == "3-Star"),
            y = ~count, name = "3-Star",
            type = "scatter", mode = "lines", fill = "tozeroy",
            fillcolor = "#5687F24D", line = list(color = "#5687F2")
          ) %>%
          add_trace(
            data = subset(complete_agg, rarity == "4-Star"),
            y = ~count, name = "4-Star",
            type = "scatter", mode = "lines", fill = "tozeroy",
            fillcolor = "#a259ec4D", line = list(color = "#a259ec")
          ) %>%
          add_trace(
            data = subset(complete_agg, rarity == "5-Star"),
            y = ~count, name = "5-Star",
            type = "scatter", mode = "lines", fill = "tozeroy",
            fillcolor = "#DEAA084D", line = list(color = "#F6C800")
          ) %>%
          layout(
            xaxis = list(
              title = "Month",
              type = "category",
              titlefont = list(
                family = "ContentFont, sans-serif",
                color = "#F3EFE4"
              )
            ),
            yaxis = list(
              title = "Number of Pulls",
                titlefont = list(
                family = "ContentFont, sans-serif",
                color = "#F3EFE4"
              )
            ),
            plot_bgcolor = "#161A3E",
            paper_bgcolor = "#161A3E",
            font = list(
              color = "#F3EFE4",
              family = "Inter, sans-serif",
              weight = "normal"
            ),    
            hovermode = "x unified",
            legend = list(
              orientation = "h",
              x = 0.5,
              y = -0.3,
              xanchor = "center",
              yanchor = "top"
            )
          )
      }
    }
  })

  # -------------------------
  # Recent Pull Item Detail Modal
  # -------------------------
  
  # Helper function to convert item name to API-friendly ID
  name_to_api_id <- function(name) {
    # Convert to lowercase, replace spaces with hyphens, remove special characters
    id <- tolower(name)
    id <- gsub(" ", "-", id)
    id <- gsub("[^a-z0-9-]", "", id)
    return(id)
  }
  
  # Helper function to fetch item data from Genshin API
  fetch_genshin_item <- function(item_name, item_type) {
    base_url <- "https://genshin.jmp.blue"
    item_id <- name_to_api_id(item_name)
    
    # Determine endpoint based on type
    endpoint <- if (tolower(item_type) == "character") "characters" else "weapons"
    
    tryCatch({
      # Fetch item details
      response <- GET(
        paste0(base_url, "/", endpoint, "/", item_id),
        query = list(lang = "en"),
        timeout(10)
      )
      
      if (status_code(response) == 200) {
        item_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
        
        # Construct image URL
        if (tolower(item_type) == "character") {
          # For characters, use gacha-card or icon-big
          image_url <- paste0(base_url, "/", endpoint, "/", item_id, "/gacha-card")
        } else {
          # For weapons, use icon
          image_url <- paste0(base_url, "/", endpoint, "/", item_id, "/icon")
        }
        
        return(list(
          success = TRUE,
          data = item_data,
          image_url = image_url
        ))
      } else {
        return(list(success = FALSE, error = "Item not found in API"))
      }
    }, error = function(e) {
      return(list(success = FALSE, error = as.character(e$message)))
    })
  }
  
  # Observer for recent pull item click
  observeEvent(input$recent_pull_clicked, {
    req(logged_in())
    req(input$recent_pull_clicked)
    
    item_info <- input$recent_pull_clicked
    item_name <- item_info$name
    item_type <- item_info$type
    item_rarity <- item_info$rarity
    item_date <- item_info$date
    item_pity <- item_info$pity
    
    # Fetch item data from API
    api_result <- fetch_genshin_item(item_name, item_type)
    
    # Generate star icons based on rarity
    star_count <- as.integer(gsub("-Star", "", item_rarity))
    star_svg <- '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 18 18" fill="none" style="vertical-align:middle;"><path d="M17.4536 6.28335C17.3987 6.12135 17.2974 5.97901 17.1624 5.8739C17.0275 5.76878 16.8647 5.7055 16.6941 5.69185L11.7058 5.29547L9.54715 0.517097C9.4784 0.363195 9.36658 0.232478 9.22519 0.140722C9.08379 0.048966 8.91887 9.26441e-05 8.75031 1.31581e-07C8.58175 -9.2381e-05 8.41677 0.0486 8.27527 0.140201C8.13377 0.231801 8.02181 0.362396 7.9529 0.516222L5.79427 5.29547L0.805898 5.69185C0.638296 5.70512 0.478069 5.76641 0.344384 5.86836C0.210698 5.97032 0.109217 6.10863 0.0520795 6.26675C-0.00505783 6.42487 -0.0154315 6.5961 0.0221998 6.75996C0.0598311 6.92382 0.143874 7.07337 0.264273 7.19072L3.95065 10.7843L2.6469 16.4298C2.60731 16.6007 2.62 16.7796 2.68332 16.9432C2.74663 17.1067 2.85765 17.2475 3.00198 17.3472C3.1463 17.4469 3.31725 17.501 3.49266 17.5023C3.66807 17.5036 3.83983 17.4522 3.98565 17.3547L8.75002 14.1785L13.5144 17.3547C13.6634 17.4537 13.8392 17.5046 14.0181 17.5007C14.1969 17.4968 14.3703 17.4382 14.5149 17.3328C14.6594 17.2274 14.7683 17.0802 14.8267 16.9112C14.8851 16.7421 14.8904 16.5591 14.8418 16.387L13.2414 10.787L17.2104 7.21522C17.4703 6.98072 17.5657 6.61497 17.4536 6.28335Z" fill="currentColor"/></svg>'
    stars_html <- paste(rep(star_svg, star_count), collapse = "")
    
    # Determine star color class
    star_class <- switch(item_rarity,
      "5-Star" = "stars-5",
      "4-Star" = "stars-4",
      "stars-3"
    )
    
    # Build modal content
    if (api_result$success) {
      api_data <- api_result$data
      
      # Get description based on type
      if (tolower(item_type) == "character") {
        description <- api_data$description %||% "No description available."
        title_text <- api_data$title %||% ""
        extra_info <- tagList(
          if (nzchar(title_text)) div(class = "item-detail-title", title_text),
          div(class = "item-detail-info-row",
            span(class = "info-label", "Vision:"),
            span(class = "info-value", api_data$vision %||% "Unknown")
          ),
          div(class = "item-detail-info-row",
            span(class = "info-label", "Weapon:"),
            span(class = "info-value", api_data$weapon %||% "Unknown")
          ),
          div(class = "item-detail-info-row",
            span(class = "info-label", "Nation:"),
            span(class = "info-value", api_data$nation %||% "Unknown")
          )
        )
      } else {
        # Weapon
        description <- api_data$passiveDesc %||% "No description available."
        extra_info <- tagList(
          div(class = "item-detail-info-row",
            span(class = "info-label", "Type:"),
            span(class = "info-value", api_data$type %||% "Unknown")
          ),
          div(class = "item-detail-info-row",
            span(class = "info-label", "Base ATK:"),
            span(class = "info-value", api_data$baseAttack %||% "Unknown")
          ),
          div(class = "item-detail-info-row",
            span(class = "info-label", "Sub Stat:"),
            span(class = "info-value", api_data$subStat %||% "None")
          ),
          if (!is.null(api_data$passiveName) && nzchar(api_data$passiveName)) {
            div(class = "item-detail-passive",
              span(class = "passive-label", "Passive: "),
              span(class = "passive-name", api_data$passiveName)
            )
          }
        )
      }
      
      modal_content <- div(class = "item-detail-modal",
        div(class = "item-detail-header",
          div(class = "item-detail-image-container",
            tags$img(
              src = api_result$image_url,
              class = "item-detail-image",
              alt = item_name,
              onerror = "this.style.display='none'; this.nextElementSibling.style.display='flex';"
            ),
            div(class = "item-detail-image-fallback", style = "display: none;",
              HTML('<svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1"><rect x="3" y="3" width="18" height="18" rx="2" ry="2"></rect><circle cx="8.5" cy="8.5" r="1.5"></circle><polyline points="21 15 16 10 5 21"></polyline></svg>')
            )
          ),
          div(class = "item-detail-header-info",
            div(class = "item-detail-name", item_name),
            div(class = paste("item-detail-stars", star_class), HTML(stars_html)),
            extra_info
          )
        ),
        div(class = "item-detail-body",
          div(class = "item-detail-section",
            div(class = "section-title", "Description"),
            div(class = "section-content", description)
          ),
          div(class = "item-detail-pull-info",
            div(class = "pull-info-item",
              span(class = "pull-info-label", "Pull Date"),
              span(class = "pull-info-value", item_date)
            ),
            div(class = "pull-info-item",
              span(class = "pull-info-label", "Type"),
              span(class = "pull-info-value", item_type)
            ),
            div(class = "pull-info-item",
              span(class = "pull-info-label", "Pity"),
              span(class = paste("pull-info-value pity-value", star_class), item_pity)
            )
          )
        )
      )
    } else {
      # API failed, show basic info only
      modal_content <- div(class = "item-detail-modal",
        div(class = "item-detail-header",
          div(class = "item-detail-header-info",
            div(class = "item-detail-name", item_name),
            div(class = paste("item-detail-stars", star_class), HTML(stars_html))
          )
        ),
        div(class = "item-detail-body",
          div(class = "item-detail-section",
            div(class = "section-content api-error", 
              "Unable to fetch detailed information from the API."
            )
          ),
          div(class = "item-detail-pull-info",
            div(class = "pull-info-item",
              span(class = "pull-info-label", "Pull Date"),
              span(class = "pull-info-value", item_date)
            ),
            div(class = "pull-info-item",
              span(class = "pull-info-label", "Type"),
              span(class = "pull-info-value", item_type)
            ),
            div(class = "pull-info-item",
              span(class = "pull-info-label", "Pity"),
              span(class = paste("pull-info-value pity-value", star_class), item_pity)
            )
          )
        )
      )
    }
    
    showModal(modalDialog(
      title = div(class = "item-detail-modal-header",
        HTML('<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"></circle><line x1="12" y1="16" x2="12" y2="12"></line><line x1="12" y1="8" x2="12.01" y2="8"></line></svg>'),
        span("Item Details")
      ),
      modal_content,
      footer = actionButton("close_item_detail", "Close", class = "btn-modal-close"),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$close_item_detail, {
    removeModal()
  })
}
shinyApp(ui, server)