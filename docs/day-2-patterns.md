# Day 2: Core Messaging Patterns

Visual diagrams of the Enterprise Integration Patterns implemented in Chronicle.

## Message Channel (Point-to-Point)

A Point-to-Point Channel ensures that only one receiver consumes any given message.

```mermaid
flowchart LR
    subgraph Sender
        P[Producer]
    end
    
    subgraph Channel["Point-to-Point Channel"]
        Q[("Queue<br/>FIFO")]
    end
    
    subgraph Receivers
        C1[Consumer 1]
        C2[Consumer 2]
        C3[Consumer 3]
    end
    
    P -->|send| Q
    Q -.->|"receive<br/>(only one gets it)"| C1
    Q -.-> C2
    Q -.-> C3
    
    style Q fill:#f9f,stroke:#333,stroke-width:2px
```

**Key Properties:**
- Messages are delivered to exactly ONE consumer
- FIFO ordering preserved
- Async send, sync receive with timeout

---

## Messaging Gateway

The Gateway encapsulates messaging-specific code, exposing only domain operations.

```mermaid
flowchart TB
    subgraph Application["Application Code"]
        R[Router]
        S[Services]
    end
    
    subgraph Gateway["Messaging Gateway"]
        GW["gateway.send_event()<br/>gateway.start_consumers()"]
    end
    
    subgraph Transports["Transport Layer (hidden)"]
        OTP["OTP Channel<br/>(in-process)"]
        RMQ["RabbitMQ<br/>(distributed)"]
    end
    
    R --> GW
    S --> GW
    GW -->|"config: otp"| OTP
    GW -->|"config: rabbitmq"| RMQ
    
    style GW fill:#9cf,stroke:#333,stroke-width:2px
    style OTP fill:#bfb,stroke:#333
    style RMQ fill:#fbf,stroke:#333
```

**Key Properties:**
- Application code is transport-agnostic
- Configuration determines transport at runtime
- Same interface for local and distributed messaging

---

## Message Endpoint

An Endpoint connects an application to a messaging channel. Chronicle can run as different endpoint types.

```mermaid
flowchart LR
    subgraph Producer["Producer Endpoint"]
        HTTP[HTTP API]
        PE[Event Creator]
    end
    
    subgraph Channel
        CH[("Message<br/>Channel")]
    end
    
    subgraph Consumer["Consumer Endpoint"]
        CE[Event Processor]
        ST[(Store)]
    end
    
    HTTP --> PE
    PE -->|send| CH
    CH -->|receive| CE
    CE --> ST
    
    style PE fill:#fbb,stroke:#333,stroke-width:2px
    style CE fill:#bfb,stroke:#333,stroke-width:2px
```

**Chronicle Modes:**
```mermaid
flowchart TB
    subgraph Full["CHRONICLE_MODE=full"]
        F1[Producer] --> FC[Channel] --> F2[Consumer]
    end
    
    subgraph Prod["CHRONICLE_MODE=producer"]
        P1[Producer] --> PC[Channel]
        PC -.->|"to external<br/>consumers"| EXT1[...]
    end
    
    subgraph Cons["CHRONICLE_MODE=consumer"]
        EXT2[...] -.->|"from external<br/>producers"| CC[Channel]
        CC --> C1[Consumer]
    end
```

---

## Competing Consumers

Multiple consumers compete for messages, enabling parallel processing and load balancing.

```mermaid
flowchart LR
    subgraph Producers
        P1[Producer 1]
        P2[Producer 2]
    end
    
    subgraph Channel
        Q[("Queue")]
    end
    
    subgraph Consumers["Competing Consumers"]
        C1["Consumer 1<br/>🔄 Processing"]
        C2["Consumer 2<br/>⏳ Waiting"]
        C3["Consumer 3<br/>⏳ Waiting"]
    end
    
    P1 & P2 -->|send| Q
    Q -->|"poll"| C1
    Q -.->|"poll (empty)"| C2
    Q -.->|"poll (empty)"| C3
    
    style Q fill:#f9f,stroke:#333,stroke-width:2px
    style C1 fill:#bfb,stroke:#333
```

**Message Distribution:**
```mermaid
sequenceDiagram
    participant P as Producer
    participant Q as Queue
    participant C1 as Consumer 1
    participant C2 as Consumer 2
    participant C3 as Consumer 3
    
    P->>Q: Event A
    P->>Q: Event B
    P->>Q: Event C
    
    par Competing for messages
        C1->>Q: poll
        C2->>Q: poll
        C3->>Q: poll
    end
    
    Q-->>C1: Event A ✓
    Q-->>C2: Event B ✓
    Q-->>C3: Event C ✓
    
    Note over C1,C3: Each message processed<br/>by exactly ONE consumer
```

---

## Chronicle Architecture (Day 2)

Complete view of Chronicle's messaging architecture:

```mermaid
flowchart TB
    subgraph External["External World"]
        CLI[curl / HTTP Client]
    end
    
    subgraph Chronicle["Chronicle Application"]
        subgraph Producer["Producer Side"]
            API[HTTP Router]
            GWP["Gateway<br/>(send)"]
        end
        
        subgraph Transport["Transport Layer"]
            OTP["OTP Channel"]
            RMQ["RabbitMQ Queue"]
        end
        
        subgraph Consumer["Consumer Side"]
            GWC["Gateway<br/>(receive)"]
            CP["Consumer Pool<br/>(competing)"]
            Store[(ETS Store)]
        end
    end
    
    CLI -->|"POST /events"| API
    API --> GWP
    GWP -->|"otp"| OTP
    GWP -->|"rabbitmq"| RMQ
    OTP --> GWC
    RMQ --> GWC
    GWC --> CP
    CP --> Store
    CLI -->|"GET /events"| API
    API --> Store
    
    style GWP fill:#9cf,stroke:#333,stroke-width:2px
    style GWC fill:#9cf,stroke:#333,stroke-width:2px
    style OTP fill:#bfb,stroke:#333
    style RMQ fill:#fbf,stroke:#333
```

---

## Pipes and Filters

Divides processing into composable, independent steps connected by channels.

```mermaid
flowchart LR
    subgraph Input
        REQ[HTTP Request]
    end
    
    subgraph Pipeline["Processing Pipeline"]
        F1["validate_required()"]
        F2["trim_fields()"]
        F3["normalize_actor()"]
        F4["add_correlation_id()"]
        F5["enrich_from_entity()"]
    end
    
    subgraph Registry
        ES[("Entity Store")]
    end
    
    subgraph Output
        GW[Gateway]
    end
    
    REQ -->|raw event| F1
    F1 -->|pipe| F2
    F2 -->|pipe| F3
    F3 -->|pipe| F4
    F4 -->|pipe| F5
    F5 -->|enriched| GW
    
    ES -.->|lookup| F5
    
    style F1 fill:#fbb
    style F2 fill:#fbf
    style F3 fill:#bfb
    style F4 fill:#bbf
    style F5 fill:#ff9
    style ES fill:#9cf
```

**Key Properties:**
- Each filter is independent and reusable
- Filters can validate, transform, or enrich
- Pipeline can reject events that fail validation
- Entity enrichment adds metadata from registered entities

**Filter Types:**
- **Validation**: `validate_required()`, `validate_actor_email()`
- **Normalization**: `normalize_actor()`, `trim_fields()`
- **Enrichment**: `add_correlation_id()`, `enrich_from_entity()`
- **Logging**: `log_event()`, `log_debug()`

---

## Pattern Relationships

How the patterns work together:

```mermaid
mindmap
  root((Chronicle))
    Message Channel
      Point-to-Point
      FIFO Queue
      Async Send
      Sync Receive
    Messaging Gateway
      Transport Abstraction
      OTP Backend
      RabbitMQ Backend
      Config-driven
    Message Endpoint
      Producer Mode
      Consumer Mode
      Full Mode
    Competing Consumers
      Parallel Processing
      Load Distribution
      Exactly-once Delivery
    Pipes and Filters
      Composable Processing
      Validation Filters
      Enrichment Filters
      Entity Registry
```

---

## References

- [Message Channel](https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageChannel.html)
- [Point-to-Point Channel](https://www.enterpriseintegrationpatterns.com/patterns/messaging/PointToPointChannel.html)
- [Messaging Gateway](https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessagingGateway.html)
- [Message Endpoint](https://www.enterpriseintegrationpatterns.com/patterns/messaging/MessageEndpoint.html)
- [Competing Consumers](https://www.enterpriseintegrationpatterns.com/patterns/messaging/CompetingConsumers.html)
- [Pipes and Filters](https://www.enterpriseintegrationpatterns.com/patterns/messaging/PipesAndFilters.html)

