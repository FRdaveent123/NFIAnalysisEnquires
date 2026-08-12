fr_branding_css <- tags$head(
  tags$style(HTML("

    :root {
      --fr-purple: #6E177E;
      --fr-purple-dark: #4E0F59;
      --fr-purple-light: #A884B2;
      --fr-bg-light: #FAF9FC;
      --fr-bg-panel: #FFFFFF;
      --fr-border: #DDDDE2;
      --fr-text: #2A2A2A;
    }

    body {
      background-color: var(--fr-bg-light) !important;
      color: var(--fr-text);
      font-family: 'Segoe UI', sans-serif;
    }

    /* ===============================
       HEADER
       =============================== */
    .skin-black .main-header .navbar,
    .skin-black .main-header .logo {
      background-color: var(--fr-purple) !important;
      height: 60px !important;
      line-height: 60px !important;
      padding-left: 20px !important;
      border: none !important;
      color: white !important;
      font-weight: 600 !important;
      font-size: 22px !important;
      text-shadow: 0px 1px 2px rgba(0,0,0,0.25);
    }

    .fr-header-logo {
      position: absolute;
      right: 20px;
      top: 5px;
      height: 50px;
    }

    /* Remove sidebar toggle entirely */
    .skin-black .main-header .navbar .sidebar-toggle,
    .sidebar-toggle {
      display: none !important;
    }

    body.sidebar-collapse .main-sidebar {
      margin-left: 0 !important;
    }
    body.sidebar-collapse .content-wrapper {
      margin-left: 300px !important;
    }

    .content-wrapper,
    .right-side {
      margin-top: 0 !important;
      background-color: var(--fr-bg-light) !important;
    }

    /* ===============================
       SIDEBAR
       =============================== */
    .main-sidebar {
      margin-top: 60px !important;
      background-color: white !important;
      border-right: 1px solid var(--fr-border);
      width: 300px !important;
      padding: 10px;
    }

    .sidebar-menu > li > a {
      color: var(--fr-text) !important;
      padding: 12px 20px !important;
      font-size: 15px !important;
      font-weight: 500 !important;
    }

    .sidebar-menu > li.active > a,
    .sidebar-menu > li:hover > a {
      background-color: var(--fr-purple-light) !important;
      color: white !important;
    }

    .sidebar-menu i { 
      color: var(--fr-purple) !important; 
    }

    /* Filters heading */
    .sidebar h4 {
      color: #000000 !important;
      font-weight: 700;
      margin-top: 10px;
      margin-bottom: 10px;
    }
        
    /* Open / Closed radio buttons */
    .sidebar .radio label {
      color: #333333 !important;
      font-size: 14px !important;
      font-weight: 400 !important;
    }
    
    .sidebar .radio {
      margin-top: 2px !important;
      margin-bottom: 2px !important;
    }
    
    /* Group filter controls visually */
    .sidebar .form-group {
      background-color: #F6F6F9;
      padding: 12px;
      border-radius: 8px;
      border: 1px solid #E0E0E5;
      margin-bottom: 12px;
    }

    /* Search box text + border */
    .sidebar .form-control {
      background-color: #FFFFFF !important;
      color: #000000 !important;
      border: 1px solid #8F8FA3 !important;
      border-radius: 6px !important;
    }

    .sidebar .form-control::placeholder {
      color: #6F6F6F;
    }

    .sidebar .form-control:focus {
      border-color: var(--fr-purple) !important;
      box-shadow: 0 0 0 2px rgba(110,23,126,0.15);
    }

    /* Slider label text (Age (days)) */
    .sidebar .control-label {
      color: #000000 !important;
      font-weight: 600;
    }

    /* Slider track */
    .sidebar .irs-bar,
    .sidebar .irs-bar-edge {
      background-color: var(--fr-purple) !important;
    }

    /* Slider handles */
    .sidebar .irs-handle {
      border-color: var(--fr-purple) !important;
      background-color: #FFFFFF !important;
    }

    /* Slider min/max bubbles */
    .sidebar .irs-from,
    .sidebar .irs-to,
    .sidebar .irs-single {
      background-color: var(--fr-purple) !important;
      color: white !important;
      font-size: 11px;
    }

    /* ===============================
       BOXES
       =============================== */
    .box {
      background-color: var(--fr-bg-panel) !important;
      border-radius: 10px !important;
      border: 1px solid var(--fr-border) !important;
      box-shadow: 0 2px 6px rgba(0,0,0,0.06) !important;
    }

    .box-header {
      background-color: var(--fr-purple) !important;
      color: white !important;
      font-weight: 600 !important;
      border-radius: 10px 10px 0 0 !important;
      border-bottom: 1px solid var(--fr-border) !important;
    }

    /* ===============================
       BUTTONS
       =============================== */
    .btn-default {
      background-color: var(--fr-purple) !important;
      color: white !important;
      border-radius: 6px !important;
      border: none !important;
    }

    .btn-default:hover {
      background-color: var(--fr-purple-dark) !important;
    }
    
    /* ===============================
   CHART BOX COLOURS (FR BRANDING)
   =============================== */

/* Open Requests (primary) */
.box.box-primary > .box-header {
  background-color: #6E177E !important;  /* FR Purple */
}

/* Requests by Owner */
.box.box-info > .box-header {
  background-color: #2E6F7E !important;  /* Muted teal */
}

/* Status Distribution */
.box.box-warning > .box-header {
  background-color: #2A4F8A !important;  /* Deep blue */
}

/* Organisation Distribution */
.box.box-danger > .box-header {
  background-color: #8D5A7A !important;  /* Soft plum */
}

/* Ensure header text is readable */
.box > .box-header {
  color: #FFFFFF !important;
  font-weight: 600;
}

@media (max-width: 1600px) {

  /* Shrink sidebar hard */
  .main-sidebar {
    width: 220px !important;
    padding: 6px !important;
  }

  /* Force main content to move left */
  .content-wrapper,
  .right-side {
    margin-left: 220px !important;
  }

  /* Compress sidebar menu */
  .sidebar-menu > li > a {
    padding: 8px 10px !important;
    font-size: 13px !important;
  }

  /* Compress filter blocks */
  .sidebar .form-group {
    padding: 6px !important;
    margin-bottom: 6px !important;
  }

  /* Smaller filter headings */
  .sidebar h4 {
    font-size: 13px !important;
    margin-bottom: 6px !important;
  }

  /* Smaller labels */
  .sidebar .control-label {
    font-size: 12px !important;
  }

  /* Compact inputs */
  .sidebar .form-control {
    font-size: 12px !important;
    padding: 4px 6px !important;
  }
}

/* ===============================
   COMPACT PLOTLY CHARTS (LAPTOP)
   =============================== */
@media (max-width: 1600px) {
  .plotly.html-widget {
    height: 180px !important;
  }
}

/* Reduce vertical spacing between rows */
.content .row {
  margin-bottom: 6px !important;
}

/* Reduce space between boxes */
.content .box {
  margin-bottom: 8px !important;
}

/* Tighten box headers */
.box-header {
  padding: 6px 10px !important;
  min-height: auto !important;
}

/* Tighten box bodies */
.box-body {
  padding: 8px 10px !important;
}

/* Reduce spacing between stacked chart boxes */
.content .col-md-4 .box {
  margin-bottom: 6px !important;
}

/* Table container padding */
.dataTables_wrapper {
  padding-top: 4px !important;
  padding-bottom: 4px !important;
}

.content .row {
  margin-bottom: 4px !important;
}

.content .box {
  margin-bottom: 6px !important;
}

.box-header {
  padding: 6px 10px !important;
}

.box-body {
  padding: 6px 10px !important;
}

body {
  zoom: 0.75;
}

"))
)