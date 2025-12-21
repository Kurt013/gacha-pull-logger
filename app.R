library(shiny)
library(shinytoastr)
library(DT)
library(DBI)
library(RSQLite)
library(plotly)

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
    "))
  ),
  
  # Login Panel (shown when not logged in)
  conditionalPanel(
    condition = "!output.logged_in",
    div(class = "login-page",
      div(class = "blur-overlay"),
      div(class = "login-panel",
        # Decorative border image
        img(class = "login-border", src = "assets/login_border.png"),
        
        # Login content
        div(class = "login-content",
          # Title section
          div(class = "login-title",
            div(class = "welcome-text",
              span(class = "welcome-main", "Welcome"),
              span(class = "welcome-sub", "Traveler")
            ),
            # Subtext box with decorative element
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
          
          # Input fields
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
          
          # Login button
          actionButton(
            "login_btn",
            label = "Login",
            class = "btn-login"
          )
        )
      )
    )
  ),
  
  # Main App (shown when logged in)
  conditionalPanel(
    condition = "output.logged_in",
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

        div(id = "logger-panel", class="logger-content active",
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
          # Right column
          div(class = "analytics-right",
            # Current Progress Card
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
                    span(class = "pity-max", " / 90")
                  )
                ),
                div(class = "pity-bar-container",
                  div(class = "pity-bar-bg",
                    div(class = "pity-bar-fill", style = "width: 0%", id = "pity-fill"),
                    # Soft pity indicator SVG at 74
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
                    span(class = "marker soft-pity", "Soft Pity (74)"),
                    span(class = "marker hard-pity", "Hard Pity (90)")
                  )
                )
              )
            ),
            # Pull Trends Card
            div(class = "stats-card pull-trends",
              div(class = "stats-card-header",
                HTML('
                  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none">
                    <path d="M3 3V20C3 20.2652 3.10536 20.5196 3.29289 20.7071C3.48043 20.8946 3.73478 21 4 21H21V19H5V3H3Z" fill="#161A3E"/>
                    <path d="M15.293 14.707C15.3858 14.7999 15.496 14.8737 15.6173 14.924C15.7386 14.9743 15.8687 15.0002 16 15.0002C16.1313 15.0002 16.2614 14.9743 16.3827 14.924C16.504 14.8737 16.6142 14.7999 16.707 14.707L21.707 9.70697L20.293 8.29297L16 12.586L13.707 10.293C13.6142 10.2 13.504 10.1263 13.3827 10.076C13.2614 10.0257 13.1313 9.99977 13 9.99977C12.8687 9.99977 12.7386 10.0257 12.6173 10.076C12.496 10.1263 12.3858 10.2 12.293 10.293L7.293 15.293L8.707 16.707L13 12.414L15.293 14.707Z" fill="#161A3E"/>
                  </svg>
                '),
                span("Pull Trends")
              ),
              div(class = "trends-chart-area",
                plotlyOutput("pull_trends_chart", width = "100%", height = "277.163px")
              )
            )
          )
        )
      )
    )
  ) # End of conditionalPanel for main app
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  # ---- Authentication state ----
  logged_in <- reactiveVal(FALSE)
  
  # Output for conditionalPanel
  output$logged_in <- reactive({
    logged_in()
  })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  # Login handler
  observeEvent(input$login_btn, {
    username <- input$login_username
    password <- input$login_password
    
    # Authentication
    if (identical(username, "Kurt013") && identical(password, "@GenshinImpact13")) {
      logged_in(TRUE)
      shinytoastr::toastr_success("Welcome, Traveler!", progressBar = TRUE, showMethod = "slideDown")
    } else {
      shinytoastr::toastr_error("Invalid username or password.", progressBar = TRUE, showMethod = "slideDown")
    }
  })
  
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

  # Luck Index (expected pity / average pity)
  output$luck_index <- renderText({
    df <- data()

    if (is.null(df) || nrow(df) == 0) {
      return("N/A")
    }

    five_stars <- df[df$rarity == "5-Star", ]
    if (nrow(five_stars) == 0) return("N/A")
    avg_pity <- mean(five_stars$pity, na.rm = TRUE)
    luck_index <- 75 / avg_pity
    sprintf("%.2f%%", luck_index)
  })

  # Average Pity
  output$avg_pity <- renderText({
    df <- data()
    five_stars <- df[df$rarity == "5-Star", ]
    if (nrow(five_stars) == 0) return("N/A")
    round(mean(five_stars$pity, na.rm = TRUE))
  })

  # Current Pity (pulls left until hard pity for selected banner)
  output$current_pity <- renderText({
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) return(0)
    banner_df <- df[df$banner == banner_filter, ]
    if (nrow(banner_df) == 0) return(0)
    # Get the pity value of the latest (most recent) entry for this banner
    latest_pity <- banner_df$pity[1]
    if (is.null(latest_pity) || is.na(latest_pity)) return(0)
    latest_pity
  })

  # Pity pulls ago
  output$pity_pulls_ago <- renderText({
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) return(0)
    banner_df <- df[df$banner == banner_filter, ]
    if (nrow(banner_df) == 0) return(0)
    # Use the pity value of the most recent entry for this banner
    latest_pity <- banner_df$pity[1]
    pulls_left <- 90 - latest_pity
    pulls_left
  })

  # Update pity bar fill width
  observe({
    df <- data()
    banner_filter <- input$progress_banner
    if (is.null(banner_filter)) {
      pity <- 0
    } else {
      banner_df <- df[df$banner == banner_filter, ]
      if (nrow(banner_df) == 0) {
        pity <- 0
      } else {
        # Use the pity value of the latest (most recent) entry for this banner
        pity <- banner_df$pity[1]
        if (is.null(pity) || is.na(pity)) pity <- 0
      }
    }
    # Calculate percentage for progress bar (empty at 0, full at 90)
    pct <- min(100, max(0, (pity / 90) * 100))
    session$sendCustomMessage("updatePityBar", list(width = pct))
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
    # SVG star icon
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
      div(class = paste("recent-pull-item", border_color),
        span(class = "pull-stars", stars_html),
        span(class = "pull-name", row$name),
        span(class = paste("pull-pity", if (row$rarity == "5-Star") "stars-5" else "stars-4"), row$pity)
      )
    })
    do.call(tagList, pull_items)
  })

output$pull_trends_chart <- renderPlotly({
    df <- data()
    if (nrow(df) == 0) {
      plotly_empty(type = "scatter", mode = "lines") %>%
        layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Number of Pulls"),
          annotations = list(
            list(
              text = "No data available",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5, showarrow = FALSE
            )
          )
        )
    } else {
      # Filter by selected banner
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
                family = "ContentFont, sans-serif",  # or your custom font
                color = "#F3EFE4"
              )
            ),
            yaxis = list(
              title = "Number of Pulls",
                titlefont = list(
                family = "ContentFont, sans-serif",  # or your custom font
                color = "#F3EFE4"
              )
            ),
            plot_bgcolor = "#161A3E",                  # plot area background
            paper_bgcolor = "#161A3E",                 # full figure background
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
        # Filter by selected banner
        banner_filter <- input$progress_banner
        if (!is.null(banner_filter) && banner_filter != "") {
          df <- df[df$banner == banner_filter, , drop = FALSE]
        }

        df$month <- format(as.Date(df$pull_date), "%Y-%m")
        # Count pulls per rarity per month independently
        library(dplyr)
        agg <- df %>%
          group_by(month, rarity) %>%
          summarise(count = n(), .groups = "drop")
        # Ensure all combinations exist
        all_months <- sort(unique(df$month))
        all_rarities <- c("3-Star", "4-Star", "5-Star")
        complete_agg <- tidyr::complete(
          agg,
          month = all_months,
          rarity = all_rarities,
          fill = list(count = 0)
        )
        # Plot each rarity as its own area (not stacked)
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
                family = "ContentFont, sans-serif",  # or your custom font
                color = "#F3EFE4"
              )
            ),
            yaxis = list(
              title = "Number of Pulls",
                titlefont = list(
                family = "ContentFont, sans-serif",  # or your custom font
                color = "#F3EFE4"
              )
            ),
            plot_bgcolor = "#161A3E",                  # plot area background
            paper_bgcolor = "#161A3E",                 # full figure background
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

}

shinyApp(ui, server)