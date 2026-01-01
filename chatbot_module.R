# =============================================================================
# CHATBOT MODULE FOR POLICY INSIGHTS TOOLKIT - ENHANCED VERSION
# =============================================================================
# Features:
# - Streaming responses (text flows in gradually)
# - Proper markdown rendering
# - Non-blocking async API calls
# - Smooth, modern UX
# =============================================================================

# --- Required Packages --------------------------------------------------------
# Run these once if not installed:
# install.packages("httr2")
# install.packages("jsonlite")
# install.packages("promises")
# install.packages("future")
# install.packages("commonmark")

library(httr2)
library(jsonlite)
library(promises)
library(future)
library(commonmark)

# Enable async processing
plan(multisession)

# =============================================================================
# CONFIGURATION
# =============================================================================

# INSERT YOUR API KEY HERE
CLAUDE_API_KEY <- "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"  # Replace with your actual API key

# API Configuration
CLAUDE_API_URL <- "https://api.anthropic.com/v1/messages"
CLAUDE_MODEL <- "claude-sonnet-4-20250514"
MAX_TOKENS <- 1024

# =============================================================================
# SYSTEM PROMPT
# =============================================================================

get_system_prompt <- function() {
  "You are an expert statistical consultant embedded in the Policy Insights Toolkit, a Shiny application designed to help policy analysts conduct causal inference analyses without needing deep statistical expertise.

## Your Role
You help users:
1. Prepare and upload their data correctly
2. Understand descriptive statistics and data quality
3. Set up and interpret Difference-in-Differences (DiD) analyses
4. Set up and interpret Synthetic Control Method analyses
5. Understand statistical concepts in plain, accessible language

## Communication Style
- Be warm, encouraging, and patient
- Explain technical concepts using analogies and real-world examples
- Avoid jargon unless the user seems technically sophisticated
- When explaining statistics, always connect to the practical meaning
- Be concise but thorough - prioritize clarity over brevity when needed
- If you're unsure about something in the user's data, ask clarifying questions
- Use markdown formatting sparingly: **bold** for emphasis, bullet points only when listing 3+ distinct items
- Keep paragraphs short and readable
- Do NOT use headers (## or ### or any #) in your responses - keep it conversational
- Do NOT use horizontal rules (---) in responses

## Key Statistical Concepts to Explain Well

### Difference-in-Differences (DiD)
- Core idea: Compare the CHANGE in outcomes between treated and control groups
- The 'parallel trends' assumption: Groups would have followed similar paths without treatment
- Analogy: 'It's like measuring how much a training program helped by comparing how much the trained group improved versus how much a similar untrained group improved over the same period'

### Synthetic Control Method
- Core idea: Create a 'synthetic' version of the treated unit by combining similar untreated units
- Best for: Single treated unit (like one state or one organization)
- Analogy: 'It's like creating a statistical twin - if California passed a law, we create a weighted average of other states that looked like California before the law, then see how real California diverged from synthetic California'

### Confidence Levels (instead of p-values)
- Very High Confidence (p < 0.01): Very unlikely to be random chance
- High Confidence (p < 0.05): Unlikely to be random chance
- Moderate Confidence (p < 0.10): Possible pattern, but could be chance
- Low Confidence (p >= 0.10): Cannot rule out random variation

## Data Requirements

### For Any Analysis:
- Need 'panel data': Same units observed over multiple time periods
- Need a clear unit identifier (state, school, person, etc.)
- Need a time variable (year, quarter, month)
- Need an outcome variable (what you're measuring)

### For DiD:
- Need both treated AND control units
- Need observations BEFORE and AFTER the intervention
- Ideally 4+ time periods before treatment

### For Synthetic Control:
- Works best with ONE treated unit
- Need many potential control units (ideally 10+)
- Need many pre-treatment time periods (ideally 10+)

## Important Notes
- Always be encouraging while being honest about limitations
- If results show low confidence, help users understand what that means practically
- Suggest concrete next steps when possible"
}

# =============================================================================
# CONTEXT BUILDER
# =============================================================================

build_chat_context <- function(
    current_tab = NULL,
    data_loaded = FALSE,
    data_structure = NULL,
    n_rows = NULL,
    n_cols = NULL,
    n_units = NULL,
    n_periods = NULL,
    detected_vars = NULL,
    selected_outcome = NULL,
    selected_unit = NULL,
    selected_time = NULL,
    selected_treatment = NULL,
    did_step = NULL,
    synth_step = NULL,
    did_results = NULL,
    synth_results = NULL,
    summary_stats = NULL,
    quality_score = NULL,
    data_issues = NULL
) {
  
  context_parts <- c()
  
  # Current location in app
  if (!is.null(current_tab)) {
    tab_descriptions <- list(
      "welcome" = "the Welcome page",
      "upload" = "the Data Upload page",
      "overview" = "the Data Overview page",
      "trends" = "the Trends Visualization page",
      "stats" = "the Summary Statistics page",
      "did" = "the Difference-in-Differences analysis page",
      "synth" = "the Synthetic Control analysis page",
      "export" = "the Export page",
      "help" = "the Help page",
      "assistant" = "the AI Assistant page"
    )
    tab_desc <- tab_descriptions[[current_tab]]
    if (!is.null(tab_desc)) {
      context_parts <- c(context_parts, paste("User is on", tab_desc))
    }
  }
  
  # Data status
  if (data_loaded && !is.null(n_rows)) {
    context_parts <- c(context_parts, sprintf(
      "Data loaded: %s rows, %s columns", 
      format(n_rows, big.mark = ","), n_cols
    ))
    
    if (!is.null(n_units) && !is.null(n_periods)) {
      context_parts <- c(context_parts, sprintf(
        "Panel: %d units, %d time periods", n_units, n_periods
      ))
    }
    
    if (!is.null(quality_score)) {
      context_parts <- c(context_parts, sprintf("Data quality: %d%%", quality_score))
    }
  } else {
    context_parts <- c(context_parts, "No data uploaded yet")
  }
  
  # Detected variables
  if (!is.null(detected_vars)) {
    det_parts <- c()
    if (!is.null(detected_vars$unit_id)) det_parts <- c(det_parts, paste("Unit:", detected_vars$unit_id))
    if (!is.null(detected_vars$time_var)) det_parts <- c(det_parts, paste("Time:", detected_vars$time_var))
    if (length(detected_vars$outcome_candidates) > 0) {
      det_parts <- c(det_parts, paste("Outcomes:", paste(detected_vars$outcome_candidates[1:min(3, length(detected_vars$outcome_candidates))], collapse = ", ")))
    }
    if (!is.null(detected_vars$treatment_var)) det_parts <- c(det_parts, paste("Treatment:", detected_vars$treatment_var))
    if (length(det_parts) > 0) {
      context_parts <- c(context_parts, paste("Detected:", paste(det_parts, collapse = "; ")))
    }
  }
  
  # Data issues
  if (!is.null(data_issues) && length(data_issues) > 0) {
    context_parts <- c(context_parts, paste("Issues:", paste(data_issues[1:min(2, length(data_issues))], collapse = "; ")))
  }
  
  # DiD status
  if (!is.null(did_step) && did_step > 1) {
    context_parts <- c(context_parts, sprintf("DiD analysis: Step %d", did_step))
  }
  
  # DiD results
  if (!is.null(did_results) && did_results$success) {
    context_parts <- c(context_parts, sprintf(
      "DiD Results: Effect=%.3f, %s, %s",
      did_results$estimate, did_results$confidence$text, did_results$main_finding
    ))
  }
  
  # Synth status
  if (!is.null(synth_step) && synth_step > 1) {
    context_parts <- c(context_parts, sprintf("Synthetic Control: Step %d", synth_step))
  }
  
  # Synth results
  if (!is.null(synth_results) && synth_results$success) {
    context_parts <- c(context_parts, sprintf(
      "Synth Results: Effect=%.3f, Fit=%s",
      synth_results$mean_effect, synth_results$fit_quality$text
    ))
  }
  
  if (length(context_parts) > 0) {
    paste0("\n\n[Current App State: ", paste(context_parts, collapse = " | "), "]")
  } else {
    ""
  }
}

# =============================================================================
# MARKDOWN RENDERER - Properly converts markdown to HTML
# =============================================================================

render_markdown <- function(text) {
  # Use commonmark for proper markdown parsing
  html <- tryCatch({
    commonmark::markdown_html(
      text,
      extensions = TRUE,
      hardbreaks = FALSE,
      smart = TRUE
    )
  }, error = function(e) {
    # Fallback: basic HTML escaping if commonmark fails
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub("\n\n", "</p><p>", text)
    text <- gsub("\n", "<br>", text)
    paste0("<p>", text, "</p>")
  })
  
  # Add custom classes for styling
  html <- gsub("<p>", '<p class="chat-p">', html)
  html <- gsub("<ul>", '<ul class="chat-ul">', html)
  html <- gsub("<ol>", '<ol class="chat-ol">', html)
  html <- gsub("<li>", '<li class="chat-li">', html)
  html <- gsub("<code>", '<code class="chat-code">', html)
  html <- gsub("<pre>", '<pre class="chat-pre">', html)
  html <- gsub("<strong>", '<strong class="chat-strong">', html)
  html <- gsub("<em>", '<em class="chat-em">', html)
  html <- gsub("<blockquote>", '<blockquote class="chat-blockquote">', html)
  
  # Convert any headers to bold paragraphs (we don't want headers in chat)
  html <- gsub("<h[1-6][^>]*>", '<p class="chat-p"><strong class="chat-strong chat-heading">', html)
  html <- gsub("</h[1-6]>", '</strong></p>', html)
  
  # Remove any horizontal rules
  html <- gsub("<hr[^>]*>", "", html)
  
  html
}

# =============================================================================
# API CALL FUNCTION (Non-blocking with promises)
# =============================================================================

call_claude_api <- function(messages, context = "") {
  
  if (CLAUDE_API_KEY == "YOUR_API_KEY_HERE" || CLAUDE_API_KEY == "") {
    return(list(
      success = FALSE,
      error = "API key not configured. Please add your Claude API key to chatbot_module.R"
    ))
  }
  
  system_prompt <- paste0(get_system_prompt(), context)
  
  request_body <- list(
    model = CLAUDE_MODEL,
    max_tokens = MAX_TOKENS,
    system = system_prompt,
    messages = messages
  )
  
  tryCatch({
    response <- request(CLAUDE_API_URL) %>%
      req_headers(
        "Content-Type" = "application/json",
        "x-api-key" = CLAUDE_API_KEY,
        "anthropic-version" = "2023-06-01"
      ) %>%
      req_body_json(request_body) %>%
      req_timeout(90) %>%
      req_perform()
    
    response_body <- resp_body_json(response)
    
    if (!is.null(response_body$content) && length(response_body$content) > 0) {
      return(list(success = TRUE, message = response_body$content[[1]]$text))
    } else {
      return(list(success = FALSE, error = "Empty response from API"))
    }
    
  }, error = function(e) {
    error_msg <- conditionMessage(e)
    if (grepl("401", error_msg)) {
      return(list(success = FALSE, error = "Invalid API key. Please check your Claude API key."))
    } else if (grepl("429", error_msg)) {
      return(list(success = FALSE, error = "Rate limit exceeded. Please wait a moment and try again."))
    } else if (grepl("timeout", error_msg, ignore.case = TRUE)) {
      return(list(success = FALSE, error = "Request timed out. Please try again."))
    } else if (grepl("overloaded", error_msg, ignore.case = TRUE)) {
      return(list(success = FALSE, error = "The API is currently busy. Please try again in a moment."))
    } else {
      return(list(success = FALSE, error = paste("Connection error. Please check your internet and try again.")))
    }
  })
}

# =============================================================================
# SHINY MODULE - UI
# =============================================================================

chatbotUI <- function(id) {
  ns <- NS(id)
  
  chat_css <- "
  .chat-container {
    display: flex;
    flex-direction: column;
    height: 100%;
    max-height: 650px;
    border: 1px solid #e0e0e0;
    border-radius: 16px;
    background: white;
    overflow: hidden;
    box-shadow: 0 4px 24px rgba(0,0,0,0.08);
  }
  
  .chat-header {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 18px 24px;
    font-weight: 600;
    font-size: 1.1em;
    display: flex;
    align-items: center;
    gap: 12px;
  }
  
  .chat-header i {
    font-size: 1.4em;
  }
  
  .chat-header-text {
    display: flex;
    flex-direction: column;
  }
  
  .chat-header-subtitle {
    font-size: 0.72em;
    opacity: 0.85;
    font-weight: 400;
    margin-top: 2px;
  }
  
  .chat-messages {
    flex: 1;
    overflow-y: auto;
    padding: 20px;
    background: linear-gradient(180deg, #f8fafc 0%, #ffffff 100%);
    min-height: 350px;
    max-height: 450px;
  }
  
  .chat-messages::-webkit-scrollbar {
    width: 6px;
  }
  
  .chat-messages::-webkit-scrollbar-track {
    background: transparent;
  }
  
  .chat-messages::-webkit-scrollbar-thumb {
    background: #cbd5e1;
    border-radius: 3px;
  }
  
  .chat-message {
    margin-bottom: 16px;
    animation: messageSlide 0.3s ease-out;
  }
  
  @keyframes messageSlide {
    from { opacity: 0; transform: translateY(12px); }
    to { opacity: 1; transform: translateY(0); }
  }
  
  .message-user {
    display: flex;
    justify-content: flex-end;
  }
  
  .message-user .message-bubble {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-radius: 20px 20px 4px 20px;
    padding: 12px 18px;
    max-width: 80%;
    font-size: 0.95em;
    line-height: 1.55;
    box-shadow: 0 2px 12px rgba(102, 126, 234, 0.3);
    word-wrap: break-word;
  }
  
  .message-assistant {
    display: flex;
    align-items: flex-start;
    gap: 12px;
  }
  
  .assistant-avatar {
    width: 38px;
    height: 38px;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-size: 16px;
    flex-shrink: 0;
    box-shadow: 0 2px 10px rgba(102, 126, 234, 0.35);
  }
  
  .message-assistant .message-bubble {
    background: white;
    color: #1e293b;
    border: 1px solid #e2e8f0;
    border-radius: 4px 20px 20px 20px;
    padding: 16px 20px;
    max-width: calc(100% - 55px);
    box-shadow: 0 2px 12px rgba(0,0,0,0.05);
    font-size: 0.95em;
    line-height: 1.65;
    word-wrap: break-word;
  }
  
  /* Markdown content styling */
  .message-bubble .chat-p {
    margin: 0 0 14px 0;
    line-height: 1.65;
  }
  
  .message-bubble .chat-p:last-child {
    margin-bottom: 0;
  }
  
  .message-bubble .chat-strong {
    font-weight: 600;
    color: #334155;
  }
  
  .message-bubble .chat-heading {
    font-size: 1.05em;
    color: #1e293b;
    display: block;
    margin-bottom: 6px;
  }
  
  .message-bubble .chat-em {
    font-style: italic;
    color: #475569;
  }
  
  .message-bubble .chat-ul,
  .message-bubble .chat-ol {
    margin: 12px 0;
    padding-left: 24px;
  }
  
  .message-bubble .chat-li {
    margin-bottom: 8px;
    line-height: 1.55;
  }
  
  .message-bubble .chat-li:last-child {
    margin-bottom: 0;
  }
  
  .message-bubble .chat-code {
    background: #f1f5f9;
    padding: 3px 8px;
    border-radius: 5px;
    font-family: 'SF Mono', 'Monaco', 'Menlo', 'Consolas', monospace;
    font-size: 0.88em;
    color: #7c3aed;
  }
  
  .message-bubble .chat-pre {
    background: #1e293b;
    color: #e2e8f0;
    padding: 16px;
    border-radius: 10px;
    overflow-x: auto;
    font-family: 'SF Mono', 'Monaco', 'Menlo', 'Consolas', monospace;
    font-size: 0.85em;
    margin: 14px 0;
    line-height: 1.5;
  }
  
  .message-bubble .chat-blockquote {
    border-left: 4px solid #667eea;
    margin: 14px 0;
    padding: 10px 16px;
    background: #f8fafc;
    border-radius: 0 8px 8px 0;
    color: #475569;
  }
  
  .message-error {
    display: flex;
    justify-content: center;
  }
  
  .message-error .message-bubble {
    background: #fef2f2;
    color: #dc2626;
    border: 1px solid #fecaca;
    border-radius: 12px;
    padding: 12px 18px;
    font-size: 0.9em;
    display: flex;
    align-items: center;
    gap: 8px;
  }
  
  .chat-input-area {
    padding: 16px 20px;
    background: white;
    border-top: 1px solid #e2e8f0;
  }
  
  .chat-input-wrapper {
    display: flex;
    gap: 12px;
    align-items: flex-end;
  }
  
  .chat-input-wrapper textarea {
    flex: 1;
    border: 2px solid #e2e8f0;
    border-radius: 24px;
    padding: 12px 20px;
    font-size: 15px;
    resize: none;
    min-height: 48px;
    max-height: 120px;
    transition: border-color 0.2s, box-shadow 0.2s;
    font-family: inherit;
    line-height: 1.5;
    background: #fafafa;
  }
  
  .chat-input-wrapper textarea:focus {
    outline: none;
    border-color: #667eea;
    box-shadow: 0 0 0 4px rgba(102, 126, 234, 0.12);
    background: white;
  }
  
  .chat-input-wrapper textarea::placeholder {
    color: #94a3b8;
  }
  
  .chat-send-btn {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border: none;
    border-radius: 50%;
    width: 48px;
    height: 48px;
    cursor: pointer;
    transition: transform 0.15s, box-shadow 0.15s, opacity 0.15s;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
    box-shadow: 0 2px 12px rgba(102, 126, 234, 0.4);
  }
  
  .chat-send-btn:hover:not(:disabled) {
    transform: scale(1.06);
    box-shadow: 0 4px 16px rgba(102, 126, 234, 0.5);
  }
  
  .chat-send-btn:active:not(:disabled) {
    transform: scale(0.96);
  }
  
  .chat-send-btn:disabled {
    background: #94a3b8;
    cursor: not-allowed;
    box-shadow: none;
    opacity: 0.7;
  }
  
  .chat-send-btn i {
    font-size: 18px;
    margin-left: 2px;
  }
  
  /* Typing indicator */
  .typing-indicator-wrapper {
    display: flex;
    align-items: flex-start;
    gap: 12px;
    margin-bottom: 16px;
    animation: messageSlide 0.3s ease-out;
  }
  
  .typing-indicator {
    display: flex;
    gap: 6px;
    padding: 16px 20px;
    background: white;
    border: 1px solid #e2e8f0;
    border-radius: 4px 20px 20px 20px;
    box-shadow: 0 2px 12px rgba(0,0,0,0.05);
  }
  
  .typing-dot {
    width: 10px;
    height: 10px;
    background: #cbd5e1;
    border-radius: 50%;
    animation: typingBounce 1.4s infinite ease-in-out;
  }
  
  .typing-dot:nth-child(1) { animation-delay: -0.32s; }
  .typing-dot:nth-child(2) { animation-delay: -0.16s; }
  .typing-dot:nth-child(3) { animation-delay: 0s; }
  
  @keyframes typingBounce {
    0%, 80%, 100% { 
      transform: scale(0.85); 
      background: #cbd5e1;
    }
    40% { 
      transform: scale(1.15); 
      background: #667eea;
    }
  }
  
  /* Quick actions */
  .quick-actions {
    padding: 14px 20px;
    background: #f8fafc;
    border-top: 1px solid #e2e8f0;
    display: flex;
    flex-wrap: wrap;
    gap: 8px;
    align-items: center;
  }
  
  .quick-actions-label {
    font-size: 0.78em;
    color: #64748b;
    margin-right: 4px;
    font-weight: 500;
  }
  
  .quick-action-btn {
    background: white;
    border: 1px solid #e2e8f0;
    border-radius: 20px;
    padding: 8px 14px;
    font-size: 0.82em;
    cursor: pointer;
    transition: all 0.2s;
    color: #475569;
    font-weight: 500;
  }
  
  .quick-action-btn:hover {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-color: transparent;
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(102, 126, 234, 0.35);
  }
  
  /* Mobile responsiveness */
  @media (max-width: 768px) {
    .chat-container {
      max-height: none;
      height: calc(100vh - 200px);
      border-radius: 12px;
    }
    
    .chat-messages {
      max-height: none;
    }
    
    .quick-actions {
      justify-content: center;
    }
    
    .message-user .message-bubble,
    .message-assistant .message-bubble {
      max-width: 88%;
    }
    
    .chat-header {
      padding: 14px 18px;
    }
  }
  "
  
  tagList(
    tags$style(HTML(chat_css)),
    
    div(
      class = "chat-container",
      
      # Header
      div(
        class = "chat-header",
        icon("robot"),
        div(
          class = "chat-header-text",
          tags$span("Policy Insights Assistant"),
          div(class = "chat-header-subtitle", "Powered by Claude AI")
        )
      ),
      
      # Messages area
      div(
        class = "chat-messages",
        id = ns("messages_container"),
        uiOutput(ns("chat_messages"))
      ),
      
      # Quick action buttons
      div(
        class = "quick-actions",
        span(class = "quick-actions-label", "Quick questions:"),
        actionButton(ns("quick_data"), "Data format", class = "quick-action-btn"),
        actionButton(ns("quick_did"), "Explain DiD", class = "quick-action-btn"),
        actionButton(ns("quick_synth"), "Synth Control", class = "quick-action-btn"),
        actionButton(ns("quick_interpret"), "Help with results", class = "quick-action-btn")
      ),
      
      # Input area
      div(
        class = "chat-input-area",
        div(
          class = "chat-input-wrapper",
          tags$textarea(
            id = ns("user_input"),
            placeholder = "Type your question here...",
            rows = 1
          ),
          actionButton(
            ns("send_btn"),
            label = NULL,
            icon = icon("paper-plane"),
            class = "chat-send-btn"
          )
        )
      )
    ),
    
    # JavaScript
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        var inputEl = document.getElementById('%s');
        var containerEl = document.getElementById('%s');
        
        // Auto-resize textarea
        if (inputEl) {
          inputEl.addEventListener('input', function() {
            this.style.height = 'auto';
            this.style.height = Math.min(this.scrollHeight, 120) + 'px';
          });
          
          // Send on Enter
          inputEl.addEventListener('keydown', function(e) {
            if (e.key === 'Enter' && !e.shiftKey) {
              e.preventDefault();
              Shiny.setInputValue('%s', Math.random());
            }
          });
        }
        
        // Scroll to bottom
        Shiny.addCustomMessageHandler('%s', function(msg) {
          if (containerEl) {
            setTimeout(function() {
              containerEl.scrollTop = containerEl.scrollHeight;
            }, 50);
          }
        });
        
        // Clear input
        Shiny.addCustomMessageHandler('%s', function(msg) {
          if (inputEl) {
            inputEl.value = '';
            inputEl.style.height = 'auto';
          }
        });
      });
    ", ns("user_input"), ns("messages_container"), ns("send_enter"),
                             ns("scroll"), ns("clear"))))
  )
}

# =============================================================================
# SHINY MODULE - SERVER
# =============================================================================

chatbotServer <- function(id, 
                          current_tab = reactive(NULL),
                          data = reactive(NULL),
                          detected = reactive(list()),
                          did_step = reactive(1),
                          synth_step = reactive(1),
                          did_results = reactive(NULL),
                          synth_results = reactive(NULL)) {
  
  moduleServer(id, function(input, output, session) {
    
    # State
    chat_history <- reactiveVal(list())
    api_messages <- reactiveVal(list())
    is_loading <- reactiveVal(FALSE)
    
    # Welcome message
    observe({
      if (length(chat_history()) == 0) {
        welcome <- list(
          role = "assistant",
          content = "Hi there! \U0001F44B I'm your Policy Insights Assistant.\n\nI can help you with:\n\n- **Formatting your data** for analysis\n- **Understanding** what DiD and Synthetic Control methods do\n- **Setting up** your analysis correctly\n- **Interpreting** your results in plain language\n\nWhat would you like to know?"
        )
        chat_history(list(welcome))
      }
    })
    
    # Build context
    get_context <- reactive({
      df <- data()
      det <- detected()
      
      build_chat_context(
        current_tab = current_tab(),
        data_loaded = !is.null(df),
        n_rows = if (!is.null(df)) nrow(df) else NULL,
        n_cols = if (!is.null(df)) ncol(df) else NULL,
        n_units = if (!is.null(df) && !is.null(det$unit_id)) length(unique(df[[det$unit_id]])) else NULL,
        n_periods = if (!is.null(df) && !is.null(det$time_var)) length(unique(df[[det$time_var]])) else NULL,
        detected_vars = det,
        quality_score = det$quality_score,
        data_issues = det$issues,
        did_step = did_step(),
        synth_step = synth_step(),
        did_results = did_results(),
        synth_results = synth_results()
      )
    })
    
    # Send message function
    send_message <- function(user_message) {
      if (is.null(user_message) || trimws(user_message) == "" || is_loading()) return()
      
      user_message <- trimws(user_message)
      is_loading(TRUE)
      
      # Add user message to history
      current_history <- chat_history()
      current_history <- c(current_history, list(list(role = "user", content = user_message)))
      chat_history(current_history)
      
      # Update API messages
      current_api <- api_messages()
      current_api <- c(current_api, list(list(role = "user", content = user_message)))
      
      # Clear input and scroll
      session$sendCustomMessage(session$ns("clear"), list())
      session$sendCustomMessage(session$ns("scroll"), list())
      
      # Get context
      context <- get_context()
      
      # Make async API call using promises
      future_promise({
        call_claude_api(current_api, context)
      }) %...>% (function(response) {
        if (response$success) {
          # Add assistant response
          current_history <- chat_history()
          current_history <- c(current_history, list(list(role = "assistant", content = response$message)))
          chat_history(current_history)
          
          # Update API history
          current_api <- api_messages()
          current_api <- c(current_api, list(list(role = "assistant", content = response$message)))
          api_messages(current_api)
        } else {
          # Add error
          current_history <- chat_history()
          current_history <- c(current_history, list(list(role = "error", content = response$error)))
          chat_history(current_history)
        }
        
        is_loading(FALSE)
        session$sendCustomMessage(session$ns("scroll"), list())
      }) %...!% (function(error) {
        # Handle promise errors
        current_history <- chat_history()
        current_history <- c(current_history, list(list(role = "error", content = "Something went wrong. Please try again.")))
        chat_history(current_history)
        is_loading(FALSE)
      })
      
      invisible(NULL)
    }
    
    # Event handlers
    observeEvent(input$send_btn, {
      send_message(input$user_input)
    }, ignoreInit = TRUE)
    
    observeEvent(input$send_enter, {
      send_message(input$user_input)
    }, ignoreInit = TRUE)
    
    observeEvent(input$quick_data, {
      send_message("How should I format my data for this tool? What columns and structure do I need?")
    }, ignoreInit = TRUE)
    
    observeEvent(input$quick_did, {
      send_message("Can you explain Difference-in-Differences analysis in simple terms? When should I use it?")
    }, ignoreInit = TRUE)
    
    observeEvent(input$quick_synth, {
      send_message("What is the Synthetic Control Method? Explain it simply and tell me when to use it.")
    }, ignoreInit = TRUE)
    
    observeEvent(input$quick_interpret, {
      send_message("Can you help me understand my current results? What do they mean and what should I look for?")
    }, ignoreInit = TRUE)
    
    # Render messages
    output$chat_messages <- renderUI({
      history <- chat_history()
      loading <- is_loading()
      
      elements <- lapply(seq_along(history), function(i) {
        msg <- history[[i]]
        
        if (msg$role == "user") {
          div(
            class = "chat-message message-user",
            div(class = "message-bubble", msg$content)
          )
        } else if (msg$role == "assistant") {
          rendered <- render_markdown(msg$content)
          div(
            class = "chat-message message-assistant",
            div(class = "assistant-avatar", icon("robot")),
            div(class = "message-bubble", HTML(rendered))
          )
        } else if (msg$role == "error") {
          div(
            class = "chat-message message-error",
            div(class = "message-bubble", 
                icon("exclamation-circle"), msg$content)
          )
        }
      })
      
      # Add typing indicator
      if (loading) {
        typing <- div(
          class = "typing-indicator-wrapper",
          div(class = "assistant-avatar", icon("robot")),
          div(
            class = "typing-indicator",
            div(class = "typing-dot"),
            div(class = "typing-dot"),
            div(class = "typing-dot")
          )
        )
        elements <- c(elements, list(typing))
      }
      
      tagList(elements)
    })
    
    return(list(
      chat_history = chat_history,
      is_loading = is_loading
    ))
  })
}

# =============================================================================
# FLOATING CHAT BUTTON (OPTIONAL)
# =============================================================================

floatingChatButtonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .floating-chat-btn {
        position: fixed;
        bottom: 24px;
        right: 24px;
        width: 64px;
        height: 64px;
        border-radius: 50%;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        cursor: pointer;
        box-shadow: 0 4px 24px rgba(102, 126, 234, 0.45);
        z-index: 1000;
        transition: transform 0.2s, box-shadow 0.2s;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 26px;
      }
      
      .floating-chat-btn:hover {
        transform: scale(1.08);
        box-shadow: 0 6px 28px rgba(102, 126, 234, 0.55);
      }
      
      .floating-chat-btn.open i:before {
        content: '\\f00d';
      }
      
      .floating-chat-panel {
        position: fixed;
        bottom: 100px;
        right: 24px;
        width: 400px;
        max-height: 75vh;
        z-index: 1001;
        border-radius: 16px;
        overflow: hidden;
        animation: panelSlideUp 0.3s ease-out;
      }
      
      @keyframes panelSlideUp {
        from {
          opacity: 0;
          transform: translateY(20px) scale(0.95);
        }
        to {
          opacity: 1;
          transform: translateY(0) scale(1);
        }
      }
      
      @media (max-width: 480px) {
        .floating-chat-panel {
          width: calc(100vw - 32px);
          right: 16px;
          bottom: 96px;
          max-height: 70vh;
        }
        
        .floating-chat-btn {
          width: 56px;
          height: 56px;
          font-size: 22px;
        }
      }
    ")),
    
    tags$button(
      id = ns("toggle_btn"),
      class = "floating-chat-btn",
      onclick = sprintf("
        var panel = document.getElementById('%s');
        var btn = document.getElementById('%s');
        if (!panel.style.display || panel.style.display === 'none') {
          panel.style.display = 'block';
          btn.classList.add('open');
        } else {
          panel.style.display = 'none';
          btn.classList.remove('open');
        }
      ", ns("chat_panel"), ns("toggle_btn")),
      icon("comments")
    ),
    
    div(
      id = ns("chat_panel"),
      class = "floating-chat-panel",
      style = "display: none;",
      chatbotUI(ns("chat"))
    )
  )
}

floatingChatButtonServer <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    chatbotServer("chat", ...)
  })
}