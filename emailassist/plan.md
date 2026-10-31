Here is a comprehensive, step-by-step implementation plan designed for an AI agent (or yourself working alongside GitHub Copilot in VS Code) to build your Swedish housing cooperative (*bostadsrättsförening* / BRF) email assistant.

## **Part 1: Project Overview & Architecture**

### **1\. Purpose**

To automate the analysis, categorization, action-item extraction, and drafting of responses for Proton Mail emails related to your Swedish housing cooperative, while ensuring absolute human oversight through a local staging file and interactive approval loop.

### **2\. Design Summary**

> * **Read-Only Access:** An off-the-shelf, read-only IMAP MCP server connects to your local Proton Mail Bridge to search and read emails safely.  
> * **Custom Staging MCP:** A lightweight, custom Scala 3 MCP server (brf-mail-mcp) exposes a typed tool (propose\_action) to safely record proposed actions into a local JSON staging file.  
> * **Human-in-the-Loop Execution:** A traditional Scala CLI script reads the approved staging file, presents each action interactively in your terminal (\[y/n\]), and executes verified changes.  
> * **Agent Domain Context:** Workspace instructions (copilot-instructions.md) provide the agent with board roles, Swedish business terminology, and folder mapping rules.

### **3\. Core Requirements**

> * Ubuntu Linux environment.  
> * Proton Mail Bridge running locally (e.g., 127.0.0.1:1143).  
> * Scala 3 / scala-cli / VS Code with Metals and GitHub Copilot.  
> * An existing read-only IMAP MCP server binary/package (e.g., node/python-based open-source IMAP MCP).

### **4\. Software Components Matrix**

| Component | Technology / Tool | Role |
| :---- | :---- | :---- |
| **Mail Backend** | Proton Mail Bridge | Exposes IMAP/SMTP locally. |
| **Read-Only Tool** | Open-source IMAP MCP | Provides advanced email search, thread parsing, and reading. |
| **Staging Tool** | Custom Scala 3 \+ fast-mcp-scala | Exposes propose\_action MCP tool for the agent. |
| **Staging Store** | .brf/pending\_actions.json | Safely buffers proposed moves and replies. |
| **Execution Loop** | Scala CLI script | Interactive terminal prompt for human review and execution. |
| **Agent Context** | Markdown instructions | Guides LLM behavior, Swedish phrasing, and board rules. |

## **Part 2: Step-by-Step Implementation Plan**

### **Phase 1: Project Scaffolding & Directory Structure**

*Set up the workspace folder and configuration files.*

> 1. Create the project root directory structure:  
>    Bash  
>    mkdir \-p brf-mail-assistant/{.vscode,.github,brf-context,src/main/scala/brf/mcp,src/main/scala/brf/proton,src/main/scala/brf/model,.brf}

> 2. Initialize the project.scala configuration file for scala-cli with necessary dependencies (including fast-mcp-scala, JSON libraries like Circe or uJson, and Java/Jakarta Mail for execution).

### **Phase 2: Domain Models & Staging Logic**

*Implement the data structures that tie the agent's proposals to the execution script.*

> 1. Create src/main/scala/brf/model/EmailModels.scala:  
   * Define case classes for ActionProposal (actionType, messageId, sourceFolder, destinationFolder, draftContent, reason).  
   * Define codecs (e.g., uJson/Circe) to read/write .brf/pending\_actions.json.  
> 2. Implement a thread-safe StagingManager utility to append proposals to the local file.

### **Phase 3: Build the Custom Scala MCP Server (brf-mail-mcp)**

*Implement the MCP tool server using fast-mcp-scala.*

> 1. Create src/main/scala/brf/mcp/Main.scala:  
   * Set up McpServerApp using Stdio.  
   * Implement the @Tool annotated method proposeAction(proposal: ActionProposal): String.  
   * Add validation checks (e.g., ensure valid folder names or non-empty message IDs) returning descriptive error strings on failure for agent self-correction.  
> 2. Verify the server compiles and launches locally via scala-cli run ..

### **Phase 4: Configure Agent Skill & Domain Rules**

*Write the system instructions and knowledge base for the agent.*

> 1. Create .github/copilot-instructions.md:  
   * Define the persona: AI assistant for a Swedish BRF.  
   * List board members, contact emails, and responsibilities (*ordförande, kassör, fastighetsansvarig*).  
   * Define Proton Mail folder structure mapping rules (Ekonomi/Fakturor, Ansökningar/Andrahandsuthyrning, etc.).  
   * Enforce language rules: **Always communicate and draft responses in professional Swedish**.  
> 2. Populate brf-context/ markdown files with typical FAQ data or operational guidelines if needed.

### **Phase 5: VS Code & MCP Integration**

*Wire everything together inside VS Code.*

> 1. Create .vscode/mcp.json:  
   * Register the read-only IMAP MCP server (configured with your read-only Proton Bridge credentials).  
   * Register your custom Scala MCP server (brf-mail-mcp) pointing to scala-cli run ..  
> 2. Open the workspace in VS Code and verify that Copilot Agent Mode recognizes both the IMAP tools and your custom propose\_action tool.

### **Phase 6: The Scala CLI Review & Execution Script**

*Build the terminal interface that processes staging actions.*

> 1. Create src/main/scala/brf/execution/ExecutionRunner.scala (or a standalone script execute.scala):  
   * Reads .brf/pending\_actions.json.  
   * Iterates through each proposed action, printing human-readable details (Subject, From, Reason, Proposed Action).  
   * Prompts user input: \[Y\]es / \[N\]o / \[E\]dit / \[Q\]uit.  
   * Uses Java/Jakarta Mail connected to the Proton Bridge to execute approved actions (e.g., moving messages via Message-ID search or saving drafts).  
   * Clears or archives processed actions from the staging file upon completion.

### **Suggested Next Step for the Agent/User:**

Would you like to start by generating the **project.scala configuration and domain models (EmailModels.scala)**, or focus on writing the **custom MCP server implementation** first?